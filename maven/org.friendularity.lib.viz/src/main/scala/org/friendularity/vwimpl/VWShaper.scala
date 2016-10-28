/*
 *  Copyright 2016 by The Friendularity Project (www.friendularity.org).
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
package org.friendularity.vwimpl

import akka.actor.Actor
import com.jme3.material.Material
import com.jme3.math.{Quaternion, Vector3f, ColorRGBA}
import com.jme3.scene.shape.{Quad, Sphere}

import com.jme3.scene.{Node => JmeNode, Geometry, Mesh, Spatial}
import org.appdapter.core.name.{FreeIdent, Ident}
import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.render.sys.registry.RenderRegistryClient
import org.cogchar.render.trial.TextSpatialFactory
import org.friendularity.respire.Srtw
import org.friendularity.vwmsg.{VWSCR_CamGuideNode, VWShapeAttachRq, VWShapeDetachRq, TwoPartMeshyShapeRq, VWSCR_ExistingNode, VWSCR_Node, VWShapeManipRq, Transform3D, VWMeshyShapeRq, VWClearAllShapes, VWSCR_Sphere, VWSCR_TextBox, VWShapeCreateRq, VWSCR_CellGrid, VWStageRqMsg}

import scala.collection.mutable
import scala.util.Random

/**
  * Created by Stub22 on 6/22/2016.
  */
trait PatternGridMaker extends VarargsLogging {
	// We are currently calling this *off* rndThrd, and it is working.
	def makeBigGridAndAttach_onRendThrd(rrc: RenderRegistryClient, parentNode : JmeNode) : Unit = {
		val ssNode = makeBigGridNode(rrc)
		info2("Made cellGrid node, attaching: {} to {} - which is supposed to be on rendThrd, but conditionally may " +
					"work otherwise, too.", ssNode, parentNode)
		parentNode.attachChild(ssNode) // Question:  Is this the only step that requires us to be onRendThrd?
	}
	def makeBigGridNode(rrc: RenderRegistryClient) : JmeNode = {
		val assetMgr = rrc.getJme3AssetManager(null);
		val someMat = new Material(assetMgr, "Common/MatDefs/Misc/Unshaded.j3md") // someContent.makeAlphaBlendedUnshadedMaterial(rrc, 0f, 1.0f, 0, 0.5f);
		val matPal = new MatPallete(someMat)
		val outerGuy = new OuterTestQuadsAndTextMaker(rrc, matPal)
		val srtwInst = new Srtw {
			override def getRRC = rrc
			override def getOuterGuy : OuterTestQuadsAndTextMaker = outerGuy

		}
		val uniformCount : Int = 40
		val uniformLen : Float = 800.0f

		// Decide the cross-sections of cells we want to do something with.
		// It is actually OK if these indices are outside the implied range of the deepSpace

		val chosenIdxs_X: Range = 10 to 30 by 2
		val chosenIdxs_Y: Range = 10 to 42 by 4
		val chosenIdxs_Z: Range = -5 to 30 by 5

		val ssNode = srtwInst.makeSheetspace(uniformCount, uniformLen, chosenIdxs_X, chosenIdxs_Y, chosenIdxs_Z)
		ssNode

	}
}
trait SpatMatHelper {
	protected def getTooMuchRRC : RenderRegistryClient
	val rrc = getTooMuchRRC
	val myAssetMgr = rrc.getJme3AssetManager(null);
	val myUnshMat = new Material(myAssetMgr, "Common/MatDefs/Misc/Unshaded.j3md") // someContent.makeAlphaBlendedUnshadedMaterial(rrc, 0f, 1.0f, 0, 0.5f);
	val myMatPal = new MatPallete(myUnshMat)
	val outerGuy = new OuterTestQuadsAndTextMaker(rrc, myMatPal)
	def getBrushJar : BrushJar = outerGuy.myBrushJar


}
trait VWSpatialsForShapes extends PatternGridMaker with SpatMatHelper {


	//	val tsf: TextSpatialFactory = new TextSpatialFactory(rrc)
	def makeOrExtractSpat(vwscr : VWShapeCreateRq) : Spatial = {
		vwscr match {
			case aNode : VWSCR_Node => {
				val madeNode: JmeNode = new JmeNode("generic_node_" + System.currentTimeMillis())
				madeNode
			}
			case cgNode : VWSCR_CamGuideNode => {
				val madeNode: JmeNode = new JmeNode("camGuide_node_" + System.currentTimeMillis())
				madeNode
			}
			case existingNode :	VWSCR_ExistingNode => {
				existingNode.existingNode
			}
			case txtBox : VWSCR_TextBox => {
				null
			}
			case bigGrid : VWSCR_CellGrid => {
				makeBigGridNode(getTooMuchRRC)
			}
			case twoPartMeshyRq : TwoPartMeshyShapeRq => {
				val meshDescPart = twoPartMeshyRq.getMeshyDescPart
				val geom = makeMeshFromDesc(meshDescPart)
				geom
			}
			case meshBasedRq : VWMeshyShapeRq => {
				makeMeshFromDesc(meshBasedRq)
			}
		}
	}
	def makeMeshFromDesc(meshBasedRq : VWMeshyShapeRq) : Spatial = {
		val mesh: Mesh = meshBasedRq match {
			case sph: VWSCR_Sphere => {
				// zSamp, rSamp, radius
				new Sphere(16, 16, sph.myRadius)
			}

		}
		val geomNameArb: String = "geom_from_msg_shape_" + System.currentTimeMillis()
		val geom = new Geometry(geomNameArb, mesh)
		applyMat(geom, meshBasedRq)
		applySpatialTransform(geom, meshBasedRq.getCoreParams3D.get)
		geom

	}
	def applyMat(geom : Geometry, mshShpRq : VWMeshyShapeRq) : Unit = {
		val dsc_opt : Option[ColorRGBA] = mshShpRq.getColorParam
		val dsc = dsc_opt.getOrElse(ColorRGBA.Gray)
		val brush = getBrushJar.makeBrush(dsc)
		brush.stroke(geom)
	}
	def applySpatialTransform(spat : Spatial, params : Transform3D) : Unit = {
		val pos : Vector3f = params.getPos
		spat.setLocalTranslation(pos)
		val rot : Quaternion = params.getRotQuat
		spat.setLocalRotation(rot)
		val scl : Vector3f = params.getScale
		spat.setLocalScale(scl)
	}
}

trait SyncMe {
	def syncAfterAttach_onRendThrd : Unit = { }
}
abstract class MadeSpatRecBase extends Manipable with SyncMe

case class MadeSpatRec(mySpat : Spatial, myID_opt : Option[Ident], myCreateRq : VWShapeCreateRq)
			extends MadeSpatRecBase() {
	override def getMainSpat : Spatial = mySpat
	override def getID : Ident = myID_opt.get // Will throw if myID_opt is None!

}

case class CamGuideMadeSpatRec(myGuideNode : JmeNode, myGuideID : Ident, myCreateRq : VWSCR_CamGuideNode)
			extends MadeSpatRecBase() with SyncsToCam {
	override def getMainSpat : Spatial = myGuideNode
	override def getID : Ident = myGuideID

	override def syncAfterAttach_onRendThrd : Unit = {
		syncGuideToCam_rendThrd
	}
}


trait IdentHlp {
	val uniqueR = new Random()

	val noSuffix = ""
	val idSuffix = "#id"

	def makeStampyRandyString (prefix : String, suffix : String) : String = {
		val rnum = uniqueR.nextInt(1000 * 1000)
		val tstamp = System.currentTimeMillis()
		String.format("%s%d_%06d%s", prefix, tstamp : java.lang.Long, rnum : Integer, suffix)
	}
	def makeStampyRandyIdentAnon() : Ident = {
		makeStampyRandyIdent("")
	}
	def makeStampyRandyIdent(shortLab : String) : Ident = {
		val prePre = "urn:sri_"
		val prefix : String = if (shortLab.length > 0) {prePre + shortLab + "_"} else prePre
		val uriTxt = makeStampyRandyString(prefix, idSuffix)
		val id = new FreeIdent(uriTxt)
		id
	}
}

trait 	VWShaperLogic extends PatternGridMaker with AttachHlp with IdentHlp {

	protected val myTopDeepNode : JmeNode = {
		val nodeName = makeStampyRandyString("deep_shape_parent_", noSuffix)
		val tdn = new JmeNode(nodeName)
		val rootN = getRRC.getJme3RootDeepNode(null)
		enqueueAttach(tdn, rootN)
		tdn
	}
	protected val myTopFlatNode : JmeNode = {
		val nodeName = makeStampyRandyString("flat_shape_parent_", noSuffix)
		val tfn = new JmeNode(nodeName)
		val rootN = getRRC.getJme3RootOverlayNode(null)
		enqueueAttach(tfn, rootN)
		tfn
	}

	protected val myAssetMgr = getRRC.getJme3AssetManager(null)

	val myShapeMaker = new VWSpatialsForShapes{
		override  protected def getTooMuchRRC : RenderRegistryClient = getRRC
	}

	lazy protected val myMadeSpatRecsByID = new mutable.HashMap[Ident, MadeSpatRecBase]()

	protected def detachFromParent_onRendThrd(childSpatID : Ident): Unit = {
		val spatRec : MadeSpatRecBase = myMadeSpatRecsByID.get(childSpatID).get
		val spat = spatRec.getMainSpat
		val resultFlag = spat.removeFromParent()
		info3("removeFromParent returned={} for spat={} found at shapeID={}", resultFlag : java.lang.Boolean, spat, spatRec)
	}

	protected def clearAllShapes_onRendThrd(): Unit = {
		for (madeRec <- myMadeSpatRecsByID.values) {
			val madeSpat = madeRec.getMainSpat
			val removed = madeSpat.removeFromParent()
		}
		myMadeSpatRecsByID.clear()
		myTopDeepNode.detachAllChildren()
		myTopFlatNode.detachAllChildren()
	}
	val makeIdentIfMissing : Boolean = true

	// makes the spat, attaches IDs as needed, defers attachment to parent-node as needed.
	def makeAndPlace(toMake : VWShapeCreateRq, deferAttach : Boolean): MadeSpatRecBase = {
		val madeSpat : Spatial = myShapeMaker.makeOrExtractSpat(toMake)
		val madeSpatRec : MadeSpatRecBase = registerSpat(madeSpat, toMake)
		val deferredAttachFunc : Function0[Unit] = () => {
			attachToParent_onRendThrd(madeSpat, toMake)
			// This causes an exception if the sub-cam is not attached yet
			// madeSpatRec.syncAfterAttach_onRendThrd
		}
		if (deferAttach) {
			enqueueJmeCallable(deferredAttachFunc)
		} else {
			deferredAttachFunc
		}
		madeSpatRec
	}
	protected def findMadeSpatRec(shapeID : Ident) : Option[MadeSpatRecBase] = {
		myMadeSpatRecsByID.get(shapeID)
	}
	protected def getOrMakeID_opt (toMake : VWShapeCreateRq) : Option[Ident] = {
		val clientSentID_opt = toMake.getKnownID_opt
		clientSentID_opt.orElse(if (makeIdentIfMissing) Some(makeStampyRandyIdentAnon()) else None)
	}

	// TODO:  Clarify the exact meaning and rules of this operation
	protected def registerSpat(madeSpat : Spatial, toMake : VWShapeCreateRq): MadeSpatRecBase = {
		val assignedID_opt = getOrMakeID_opt(toMake)

		if (assignedID_opt.isDefined) {
			val assignedID: Ident = assignedID_opt.get

			// Hmm.   What is this logic saying?
			// What does "expectEmptySlot" really mean?
			val existingMSR_opt  : Option[MadeSpatRecBase] = if (toMake.expectEmptySlot) {
				val previousMadeSpatRec_opt = myMadeSpatRecsByID.get(assignedID)

				if (previousMadeSpatRec_opt.isDefined) {
					warn2("Found existing shape-spat at oldShape={}, so IGNORING newShape={}",
						previousMadeSpatRec_opt, toMake)
					previousMadeSpatRec_opt
				} else None
			} else None

			existingMSR_opt.getOrElse({

				val madeSpatRec : MadeSpatRecBase = {
					toMake match {
						case cgn : VWSCR_CamGuideNode => {
							new CamGuideMadeSpatRec(madeSpat.asInstanceOf[JmeNode], assignedID, cgn)
						}
						case otherN  => {
							new MadeSpatRec(madeSpat, assignedID_opt, toMake)
						}
					}

				}
				info2("Storing madeSpatRec at id={}, rec={}", assignedID, madeSpatRec)
				myMadeSpatRecsByID.put(assignedID, madeSpatRec)
				madeSpatRec
			})
		} else new MadeSpatRec(madeSpat, assignedID_opt, toMake)
	}
	// protected def findParentNode() : Option[JmeNode]
	// Uses node at knownParentID if specified, else attaches to 2D or 3D "root" node.
	def attachToParent_onRendThrd(madeSpat : Spatial, toMake : VWShapeCreateRq) : Unit = {
		val knownParentID_opt = toMake.getKnownParentID_opt
		attachToParent_onRendThrd(madeSpat, toMake.getKnownParentID_opt, toMake.inFlatSpace)

	}
	private def attachToParent_onRendThrd(spatToAttach : Spatial, knownParentID_opt : Option[Ident], inFlatSpc : Boolean) : Unit = {
		val knownParentNode_opt : Option[JmeNode] = if (knownParentID_opt.isDefined) {
			val kparid = knownParentID_opt.get
			val knownParentMadeRec_opt = myMadeSpatRecsByID.get(kparid)
			if (knownParentMadeRec_opt.isDefined) {
				val pNode : JmeNode = knownParentMadeRec_opt.get.getMainSpat.asInstanceOf[JmeNode]
				info1("Found known parentNode: {}", pNode)
				Option(pNode)
			} else {
				warn2("Could not find expected parentNode at ID={}, for child={}", kparid, spatToAttach)
				// TODO: Probably we should force failure in this case, not attach to default
				None
			}
		} else None

		val parentToUse_opt : Option[JmeNode] = knownParentNode_opt.orElse(
			if (inFlatSpc) {
				Option(myTopFlatNode)
			} else {
				Option(myTopDeepNode)
			}
		)

		info2("Using parentNode={} for childSpatial={}", parentToUse_opt,spatToAttach)
		val dooda = parentToUse_opt.map(_.attachChild(spatToAttach))
	}
	def reattachShapeToParent_onRendThrd(attchRq : VWShapeAttachRq) : Unit = {
		val msr : MadeSpatRecBase = findMadeSpatRec(attchRq.knownID).get
		attachToParent_onRendThrd(msr.getMainSpat, attchRq.knownParentID_opt, false)
		msr.syncAfterAttach_onRendThrd
	}
	def attachBigGrid_onRendThrd(): Unit = {
		makeBigGridAndAttach_onRendThrd(getRRC, myTopDeepNode)
	}


}

class VWShaperActor(myRRC: RenderRegistryClient) extends Actor with VWShaperLogic {
	override protected def getRRC : RenderRegistryClient = myRRC

	def receive = {
		case clearAll : VWClearAllShapes => {
			val func : Function0[Unit] = () => {clearAllShapes_onRendThrd}
			enqueueJmeCallable(myRRC, func)
		}
		case detachOne : VWShapeDetachRq => {
			val func : Function0[Unit] = () => {detachFromParent_onRendThrd(detachOne.shapeID)}
			enqueueJmeCallable(myRRC, func)
		}
		case reattachOne : VWShapeAttachRq  => {
			val func : Function0[Unit] = () => {reattachShapeToParent_onRendThrd(reattachOne)}
			enqueueJmeCallable(myRRC, func)
		}
		case cellGridRq : VWSCR_CellGrid => {
			val func : Function0[Unit] = () => {attachBigGrid_onRendThrd}
			enqueueJmeCallable(myRRC, func)
		}
		case vwsrq: VWShapeCreateRq => {
			val unusedResult : MadeSpatRecBase = makeAndPlace(vwsrq, true)
		}

		case manipRq : VWShapeManipRq => {
			val shapeID : Ident = manipRq.getTgtShapeID
			val madeSpatRec_opt : Option[MadeSpatRecBase] = findMadeSpatRec(shapeID)
			val manipDesc = manipRq.getManipDesc
			val func : Function0[Unit] = () => {
				madeSpatRec_opt.get.applyManipDesc(manipDesc, this, manipRq.getStatusHandler_opt)
			}
			enqueueJmeCallable(myRRC, func)
		}
	}
}


class OuterTestQuadsAndTextMaker(myRRC : RenderRegistryClient, myMatPal : MatPallete) {
	lazy val myFirstTSF: TextSpatialFactory = new TextSpatialFactory(myRRC)
	val myBrushJar = new BrushJar(myMatPal)

	val quadMeshFiveByFive: Mesh = new Quad(5,5)

	val redQuadMaker = new MeshGeoMaker(quadMeshFiveByFive, myBrushJar.reddy)
	val orngQuadMaker = new MeshGeoMaker(quadMeshFiveByFive, myBrushJar.orange_med)

	val happyTxtMaker = new TextSpatMaker(myFirstTSF)

}