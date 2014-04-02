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
import org.friendularity.struct.{AODFactory, _};

import org.appdapter.core.name.{Ident, FreeIdent}
import org.appdapter.core.item.{Item}
import org.appdapter.core.store.{Repo, InitialBinding, ModelClient }
import org.cogchar.render.goody.dynamic.{DynamicGoody, DynamicGoodySpace, DynaShapeGoody}

class MGPropertyBinding (val myPropQN : String, val myFieldKey : String, val myValueDim : Int) extends VarargsLogging {
	// class StringPropSel(val myPropID: Ident) extends PropSel[String] { 
	def readExprAndBind(parentSpecItem : Item, resolverModelCli : ModelClient, mathSM : MathStructMapper) {
		val expr_Prop_ID = resolverModelCli.makeIdentForQName(myPropQN);
		val todoDefault = "{-987654321}";
		val exprText = parentSpecItem.getValString(expr_Prop_ID, todoDefault);
		if (exprText != todoDefault) {
			info3("Binding field {} of dim {} to expr {}", myFieldKey, myValueDim : Integer, exprText);
			mathSM.bindFieldToMathExpr(myFieldKey, myValueDim, exprText);
		} else {
			warn4("Cannot bind field {} of dim {}, no valid expr found at prop {} in parent item {}", myFieldKey, 
						myValueDim : Integer, expr_Prop_ID, parentSpecItem)
		}
	}
	def allocateResultValue(mathSM : MathStructMapper) : ArrayOfDoubles = {
		val factory = mathSM.getValueFactoryForField(myFieldKey)
		factory.make()
	}
	def readResultValue(mssh : MathSourcedStructHandle, resultValue : ArrayOfDoubles) {
		mssh.readResultField(myFieldKey, resultValue)
	}
}
object MGBindings {
	val MGPB_position = new MGPropertyBinding("hev:expr_pos_vec3f", "position", 3)
	val MGPB_orientation  = new MGPropertyBinding("hev:expr_ori_vec3f", "orient", 3)  // Which orientation form are we using?
	val MGPB_scale = new MGPropertyBinding("hev:expr_scale_vec3f", "scale", 3) // What if we want unary scale? 
	val MGPB_color = new MGPropertyBinding("hev:expr_color_vec4f", "color", 4)
	
	val allGoodyBindings = List[MGPropertyBinding](MGPB_position, MGPB_orientation, MGPB_scale, MGPB_color)
}

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
	
	def readAndBindExprs(modelCli : ModelClient, specItem : Item, bindings : List[MGPropertyBinding]) {
		for (b <- bindings) {
			b.readExprAndBind(specItem, modelCli, myMathyHandleGroup.myMapper)
		}
	}
	// This is our "slow" update path.
	// Generally *not* assumed to be on the render thread.
	// Collision with render thread should not be catastrophic, so we hope to avoid needing synchronized(this)
	// (which carries a performance penalty).  
	override def reconfigureFromSpecItem(mc : ModelClient, specItem : Item) {
		readAndBindExprs(mc, specItem, MGBindings.allGoodyBindings)
	}
	override def doFastVWorldUpdate_onRendThrd() : Unit = { 
		super.doFastVWorldUpdate_onRendThrd();
		// Read data from myMathGate, using exprs bound in the mapper, into the fields of this particular state-struct.
		// TODO:  add capability to do this only once every K loops, configured from the goody's spec item.
		myCurrentStateHandle.updateSourcedFields
		applyPositionField_onRendThrd
	}

	protected def ensureDummyShapeSetup_onRendThrd() { 
	}
	protected def applyPositionField_onRendThrd() {
		// def readCachedFieldValue(fk : FK, tgtDV : DV) 
	}
}
class MathyGoodySpace (parentDGS : DynamicGoodySpace[_], idxIntoParent : Int, specGraphID : Ident, specID : Ident)
		extends SweetDynaSpace(parentDGS, idxIntoParent, specGraphID, specID)  {
			
	val myMathSpaceFactory = new MathSpaceFactory();
	val myMathGate : MathGate = myMathSpaceFactory.makeUnscriptedMathGate();
	
	override def makeGoody(oneBasedIdx : Integer) : SweetDynaGoody = {
		new MathyGoody(oneBasedIdx, myMathGate)
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
		// Here are the QNames for these expression fields.
		
		val posVecExpr_Prop_QN = "hev:expr_pos_vec3f";
		val oriVecExpr_Prop_QN = "hev:expr_ori_vec3f";		
		val scaleVecExpr_Prop_QN = "hev:expr_scale_vec3f";  
		val colorVecExpr_Prop_QN = "hev:expr_color_vec4f";
	
		// This resolution process from QN to ID (and hence direct dependence on ModelClient mc) could be done "once", 
		// on a per-space or global basis.
		val posVecExpr_Prop_ID = mc.makeIdentForQName(posVecExpr_Prop_QN);
		val colorVecExpr_Prop_ID = mc.makeIdentForQName(colorVecExpr_Prop_QN);
		
		// From here on out, we only use Items + Idents (so we're not directly RDF/Jena dependent).
		val posVecExpr = specItem.getValString(posVecExpr_Prop_ID, "{0, 0, 0}");		
		val colorVecExpr = specItem.getValString(colorVecExpr_Prop_ID, "{0, 0, 0, 0}");
		getLogger().info("posVecExpr=" + posVecExpr + ", colorVecExpr=" + colorVecExpr);
		*/