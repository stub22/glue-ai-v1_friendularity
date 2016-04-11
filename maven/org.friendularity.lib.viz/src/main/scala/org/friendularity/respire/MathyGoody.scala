/*
 *  Copyright 2014 by The Friendularity Project (www.friendularity.org).
 * 
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 * 
 *       http://www.apache.org/licenses/LICENSE-2.0
 * 
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package org.friendularity.respire

import org.cogchar.bind.symja.{MathGate, MathSpaceFactory}
import org.friendularity.struct.{Factory, AODFactory, AODCompatFactory, MathStructMapper, ArrayOfDoubles, 
								 MathSourcedStructHandle, MathyMappedHandleGroup};

import org.appdapter.core.name.{Ident, FreeIdent}
import org.appdapter.core.item.{Item}
import org.appdapter.core.query.{InitialBinding }
import org.appdapter.core.model.{RdfNodeTranslator }
import org.appdapter.core.store.{Repo }
import org.cogchar.render.goody.dynamic.{DynamicGoody, DynamicGoodySpace, DynaShapeGoody}

import org.appdapter.fancy.log.VarargsLogging

// This binding can be used to set up expressions for any number of separate MathStructMappers,
// each of which is encapsulated in a MathyMappedHandleGroup.  The group may be used to produce
// any number of Handles, each of which contains a struct of updatable data.

class MGPropertyBinding[OutType] (val myPropQN : String, val myFieldKey : String, val myValueDim : Int, 
								val myResultFactory : AODCompatFactory[OutType]) extends VarargsLogging {
	// class StringPropSel(val myPropID: Ident) extends PropSel[String] { 
	def readExprAndBind(parentSpecItem : Item, resolverModelCli : RdfNodeTranslator, mathSM : MathStructMapper) {
		val expr_Prop_ID = resolverModelCli.makeIdentForQName(myPropQN);
		val todoDefault = "{-987654321}";
		val exprText = parentSpecItem.getValString(expr_Prop_ID, todoDefault);
		if (exprText != todoDefault) {
			info3("Binding field {} of dim {} to expr {}", myFieldKey, myValueDim : Integer, exprText);
		//	val targetResVal = myResultFactory.make
			mathSM.bindFieldToMathExpr(myFieldKey, myValueDim, exprText); // , myResultFactory); //  Some(targetResVal));
		} else {
			warn4("Cannot bind field {} of dim {}, no valid expr found at prop {} in parent item {}", myFieldKey, 
						myValueDim : Integer, expr_Prop_ID, parentSpecItem)
		}
	}
	def allocateAOD(mathSM : MathStructMapper) : ArrayOfDoubles = {
		val factory = mathSM.getValueFactoryForField(myFieldKey)
		factory.make()
	}
	def readFieldToAOD(mssh : MathSourcedStructHandle, resultValue : ArrayOfDoubles) {
		mssh.readResultFieldToAOD(myFieldKey, resultValue)
	}
	def makeVPExpr()  { 
	}
}
import com.jme3.math.Vector3f;
import com.jme3.math.ColorRGBA;
import com.jme3.math.Quaternion;

import org.friendularity.struct.{Vec3fFactory, QuaternFactory, ColorFactory}

object MGBindings {
	val MGPB_position = new MGPropertyBinding[Vector3f]("hev:expr_pos_vec3f", "position", 3, new Vec3fFactory)
	val MGPB_orientation  = new MGPropertyBinding[Quaternion]("hev:expr_ori_vec3f", "orient", 3, new QuaternFactory)  // Which orientation form are we using?
	val MGPB_scale = new MGPropertyBinding[Vector3f]("hev:expr_scale_vec3f", "scale", 3, new Vec3fFactory) // What if we want unary scale? 
	val MGPB_color = new MGPropertyBinding[ColorRGBA]("hev:expr_color_vec4f", "color", 4, new ColorFactory)
	
	val allGoodyBindings = List[MGPropertyBinding[_]](MGPB_position, MGPB_orientation, MGPB_scale, MGPB_color)
}
import org.cogchar.render.sys.registry.RenderRegistryClient;
class MathyGoody (goodyIdx : Int, val myMathGate : MathGate) extends SweetDynaGoody(goodyIdx) {
	// Each goody manages data using instances of a single datatype:  A "Struct" (= NV pair set) of
	// fields, each containing an updatable (fixed-size) array of doubles.  
	// For example : position[3], orientation[3], color[4].
	// A single "struct mapper" (per MS-Goody) holds a math expression for each field.
	// 
	// The expressions stored in the mapper are read from the properties of the superclass's specItem,
	// and may be refreshed via that pathway.  (Our "slow" update loop, in the docs).
	// TODO:  Further factor goodies so that this reading feature is *fully* orthogonal (it's already mostly so)

	val		myMathyHandleGroup = new MathyMappedHandleGroup(myMathGate)
	
	// The goody can make as many different instances of this complete "state" struct as it wants to, 
	// including representing a time sequence of past (or future!) states.  These objects may be
	// manipulated in code as needed,  while simultaneously allowing the state to interact with
	// our mathGate calculation space as needed.
	// 
	// Typically the "current" struct state will be updated (for all fields) on every openGL update callback
	// (and it will be used to update the visual state of the displayed goody).
	// This is our "fast" update loop, as discussed in design docs.
	// TODO:  Add throttling and runaway-calc management.
	
	val		myCurrentStateHandle = myMathyHandleGroup.makeHandle
	
	def readAndBindExprs(modelCli : RdfNodeTranslator, specItem : Item, bindings : List[MGPropertyBinding[_]]) {
		for (b <- bindings) {
			b.readExprAndBind(specItem, modelCli, myMathyHandleGroup.myMapper)
		}
	}
	// This reconfigure action is our "slow" update path, which we expect to be called only when the specItem
	// has changed.  It is also called to initizlize this goody the first time.
	// Generally *not* assumed to be on the render thread.
	// Collision with render thread should not be catastrophic, so we hope to avoid needing synchronized(this)
	// (which carries a performance penalty).  
	override def reconfigureFromSpecItem(mc : RdfNodeTranslator, specItem : Item) {
		readAndBindExprs(mc, specItem, MGBindings.allGoodyBindings)
	}
	override def doFastVWorldUpdate_onRendThrd(rrc : RenderRegistryClient) : Unit = { 
		super.doFastVWorldUpdate_onRendThrd(rrc);
		// Read data from myMathGate, using exprs bound in the mapper, into the fields of this particular state-struct.
		// TODO:  add capability to do this only once every K loops, configured from the goody's spec item.
		myCurrentStateHandle.updateSourcedFields
		ensureDummyShapeSetup_onRendThrd(rrc)
		ensureAttachedToParentNode_onRendThrd()
		applyCurrentState_onRendThrd()
	}
	
import com.jme3.scene.{Node, Geometry, Spatial}
import com.jme3.material.Material;
import com.jme3.scene.shape.Cylinder;
import com.jme3.scene.shape.Dome;
import com.jme3.scene.shape.Quad;
import com.jme3.material.RenderState;
import com.jme3.material.RenderState.FaceCullMode;
import com.jme3.renderer.queue.RenderQueue;

import com.jme3.math.ColorRGBA;
import com.jme3.asset.AssetManager;
import org.cogchar.render.sys.registry.RenderRegistryClient;

	protected def applyCurrentState_onRendThrd() { 
		// val posVec3f : Vector3f = myCurrentStateHandle . 
	}
	protected def ensureDummyShapeSetup_onRendThrd(rrc : RenderRegistryClient) {
		val goodyNode = getDisplayNode();
		val childList = goodyNode.getChildren
		if (childList.size == 0) {
			getLogger().info("Making dummy child for goody at index {}", getIndex() : Integer)
			attachDummyChild(rrc)
		}
	}
	private def attachDummyChild (rrc : RenderRegistryClient) {
		val goodyNode = getDisplayNode();
		val goodyIndex = getIndex()
		val geomName = "whoopeeGeom_"  + goodyIndex
		val geom  = new Geometry(geomName, new Quad(125, 75));
		val assetMgr : AssetManager = rrc.getJme3AssetManager(null);
		
		val opacity : Float = Math.sqrt(1.0f / goodyIndex).asInstanceOf[Float]
		
		val  color : ColorRGBA  = new ColorRGBA(1.0f, 0.4f, 0.05f, opacity);
		
		val mat = makeAlphaBlendedUnshadedMaterial(assetMgr, color)
		
		geom.setMaterial(mat)
		configureRenderingForSpatial(geom);  // Sets the rendering bucket and cull mode
		
		goodyNode.attachChild(geom);
		goodyNode.setLocalTranslation(60.0f * goodyIndex - 100.0f, -10.0f + 20.0f * goodyIndex , -3.0f - 1.0f * goodyIndex);
	}
	protected def applyPositionField_onRendThrd() {
		// def readCachedFieldValue(fk : FK, tgtDV : DV) 
	}

	def makeAlphaBlendedUnshadedMaterial( assetMgr : AssetManager, color : ColorRGBA) : Material = { 
		val matFaceCullMode : FaceCullMode = FaceCullMode.Off;  		// Render both sides
		val unshMat : Material = new Material(assetMgr, "Common/MatDefs/Misc/Unshaded.j3md");
		// For transparency/lucency we set the BlendMode on addtlRenderState.
		val matBlendMode : RenderState.BlendMode  = RenderState.BlendMode.Alpha;
		// But note that to get transparency, we also need to put spatials into eligible buckets
		unshMat.getAdditionalRenderState().setBlendMode(matBlendMode);
		unshMat.getAdditionalRenderState().setFaceCullMode(matFaceCullMode);
		unshMat.setColor("Color", color);
		unshMat;
	}
	def configureRenderingForSpatial(spat : Spatial) {
		val spatRenderBucket : RenderQueue.Bucket   = RenderQueue.Bucket.Transparent;
		val spatCullHint : Spatial.CullHint  = Spatial.CullHint.Never;  // Others are CullHint.Always, CullHint.Inherit
		// Setup transparency for the spatia, but note that the material blend-mode must 
		// also support transparency.
		spat.setQueueBucket(spatRenderBucket); 
		spat.setCullHint(spatCullHint);	
	}	
}
class MathyGoodySpace (parentDGS : DynamicGoodySpace[_], idxIntoParent : Int, specGraphID : Ident, specID : Ident)
		extends SweetDynaSpace(parentDGS, idxIntoParent, specGraphID, specID)  {
			
	val myMathSpaceFactory = new MathSpaceFactory();
	val myMathGate : MathGate = myMathSpaceFactory.makeUnscriptedMathGate();
	
	override def makeGoody(oneBasedIdx : Integer) : SweetDynaGoody = {
		new MathyGoody(oneBasedIdx, myMathGate)
	}
	override def doFastVWorldUpdate_onRendThrd(rrc : RenderRegistryClient) {
		super.doFastVWorldUpdate_onRendThrd(rrc)
		ensureAttachedToParentNode_onRendThrd()
	}
}


import org.appdapter.fancy.rclient.{RepoClient, RepoClientImpl}
import org.appdapter.fancy.model.{ModelClientImpl}


object MathyGoodyTest extends VarargsLogging {
	def testDynaGoodyItemLoad(repo : Repo, repoClient : RepoClient) : SweetDynaSpace = { 
		val graphQN = "ccrti:math_sheet_60";
		val spaceSpecQN = "hevi:space_01";
		val graphID = repoClient.getDefaultRdfNodeTranslator.makeIdentForQName(graphQN);
		val mathModel = repo.getNamedModel(graphID)
		val mathModelClient = new ModelClientImpl(mathModel)
		val spaceSpecItem = mathModelClient.makeItemForQName(spaceSpecQN);
		info1("Got Goody-Space-Spec Item: {}", spaceSpecItem)
		val parentDGS = null;
		val dgs = new MathyGoodySpace(parentDGS, -999, graphID, spaceSpecItem.getIdent);
		// This sets the desired size of the space, but does not actually cause the goodies to be created.
		// That happens during update() on the render thread.
		dgs.refreshFromModelClient(mathModelClient)
		info1("Loaded MathyGoodySpc: {}", dgs)
		dgs
	}	
}
/*  
	public Set<Item> getGoodySpecItems() { 
		Set<Item> specItems = new HashSet<Item>();
		return specItems;
	}
	// private	ModelClient		myCachedModelClient;
	// private	MathGate		myMathGate;
	// private	Integer			myGoodyCount;
	// Currently we need the ModelClient to resolve QNames to idents.
	public Ident getIdent() { 
		if (myGoodyID = null) {
			String qName = "h"
			myGoodyID = new 
		}
	}
	public static class Spec {
		Ident	myKind;
	}
	// This goody is knowable to the outside world by its graph-eligible ID:
	// private	Ident					myGoodyID, myTypeID;
	protected Ident getGoodyID() {
		return myGoodyID;
	} 
	*/
   	/*
		// Can be used to configure the dynaGoody initially, or to subsequently reconfig
		// TODO:  These property names need to come from ontology-linked constants.
		// 
		// We work initially with a small fixed number of dynaGoody parameter expression fields:
		// Numeric:   Position, Orientation, Scale, Color
		// These are MathGate expressions to be evaluated and re-applied on each render-pass.
	
		// The resolution process from QN to ID (and hence direct dependence on ModelClient mc) could be done "once", 
		// on a per-space or global basis.
		val posVecExpr_Prop_ID = mc.makeIdentForQName(posVecExpr_Prop_QN);
		val colorVecExpr_Prop_ID = mc.makeIdentForQName(colorVecExpr_Prop_QN);
		
		// From here on out, we only use Items + Idents (so we're not directly RDF/Jena dependent).
		val posVecExpr = specItem.getValString(posVecExpr_Prop_ID, "{0, 0, 0}");		
		val colorVecExpr = specItem.getValString(colorVecExpr_Prop_ID, "{0, 0, 0, 0}");
		getLogger().info("posVecExpr=" + posVecExpr + ", colorVecExpr=" + colorVecExpr);
		*/