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
package org.friendularity.vw.impl.shp

import akka.actor.Actor
import com.jme3.material.Material
import com.jme3.math.{ColorRGBA, Quaternion, Vector3f}
import com.jme3.scene.shape.{Quad, Sphere}
import com.jme3.scene.{Geometry, Mesh, Node => JmeNode, Spatial}
import org.appdapter.core.name.{FreeIdent, Ident}
import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.render.sys.registry.RenderRegistryClient
import org.cogchar.render.trial.TextSpatialFactory
import org.friendularity.respire.Srtw
import org.friendularity.util.IdentHlp
import org.friendularity.vw.impl.cam.SyncsToCam
import org.friendularity.vw.impl.manip.{AppliesXforms, Manipable}
import org.friendularity.vw.impl.tsk.JmeAttachHlp
import org.friendularity.vw.mprt.manip.Transform3D
import org.friendularity.vw.msg.shp.deep.{VWMatDesc, VWApplyMatToAll, VWShapeDeleteRq, VWClearAllShapes, VWSCR_CamGuideNode, VWSCR_CellGrid, VWSCR_ExistingNode, VWSCR_Node, VWMD_Sphere, VWSCR_TextBox, VWShapeAttachRq, VWShapeCreateRq, VWShapeDetachRq, VWShapeManipRq}

import scala.collection.mutable
import scala.util.Random

/**
  * Created by Stub22 on 6/22/2016.
  */

trait 	VWShaperLogic extends PatternGridMaker with JmeAttachHlp with IdentHlp {

	protected val myTopDeepNode: JmeNode = {
		val nodeName = makeStampyRandyString("deep_shape_parent_", noSuffix)
		val tdn = new JmeNode(nodeName)
		val rootN = getRRC.getJme3RootDeepNode(null)
		enqueueAttach(tdn, rootN)
		tdn
	}
	protected val myTopFlatNode: JmeNode = {
		val nodeName = makeStampyRandyString("flat_shape_parent_", noSuffix)
		val tfn = new JmeNode(nodeName)
		val rootN = getRRC.getJme3RootOverlayNode(null)
		enqueueAttach(tfn, rootN)
		tfn
	}

	protected val myAssetMgr = getRRC.getJme3AssetManager(null)

	val myShapeMaker = new VWSpatialsForShapes {
		override protected def getTooMuchRRC: RenderRegistryClient = getRRC
	}

	lazy protected val myMadeSpatRecsByID = new mutable.HashMap[Ident, MadeSpatRecBase]()

	protected def detachFromParent_onRendThrd(childSpatID: Ident): Unit = {
		val spatRec: MadeSpatRecBase = myMadeSpatRecsByID.get(childSpatID).get
		val spat = spatRec.getMainSpat
		val resultFlag = spat.removeFromParent()
		info3("removeFromParent returned={} for spat={} found at shapeID={}", resultFlag: java.lang.Boolean, spat, spatRec)
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

	val makeIdentIfMissing: Boolean = true

	// makes the spat, attaches IDs as needed, defers attachment to parent-node as needed.
	def makeAndPlace(toMake: VWShapeCreateRq, deferAttach: Boolean): MadeSpatRecBase = {
		val madeSpat: Spatial = myShapeMaker.makeOrExtractSpat(toMake)

		val xformApplier = new AppliesXforms {}
		val xformPartial = toMake.getInitXform3D_partial
		xformApplier.applyTransform_partial_runThrd(madeSpat, xformPartial)

		val madeSpatRec: MadeSpatRecBase = registerSpat(madeSpat, toMake)

		val deferredAttachFunc: Function0[Unit] = () => {
			attachToParent_onRendThrd(madeSpat, toMake)
			// This causes an exception if the sub-cam is not attached yet.
			// madeSpatRec.syncAfterAttach_onRendThrd
			// So we don't use that technique here.
			// Instead see VWCamLogic.positionGuideWhereCamIsNow
		}
		if (deferAttach) {
			enqueueJmeCallable(deferredAttachFunc)
		} else {
			deferredAttachFunc
		}
		madeSpatRec
	}

	protected def findMadeSpatRec(shapeID: Ident): Option[MadeSpatRecBase] = {
		myMadeSpatRecsByID.get(shapeID)
	}

	protected def getOrMakeID_opt(toMake: VWShapeCreateRq): Option[Ident] = {
		val clientSentID_opt = toMake.getKnownID_opt
		clientSentID_opt.orElse(if (makeIdentIfMissing) Some(makeStampyRandyIdentAnon()) else None)
	}

	// TODO:  Clarify the exact meaning and rules of this operation
	protected def registerSpat(madeSpat: Spatial, toMake: VWShapeCreateRq): MadeSpatRecBase = {
		val assignedID_opt = getOrMakeID_opt(toMake)

		if (assignedID_opt.isDefined) {
			val assignedID: Ident = assignedID_opt.get

			// Hmm.   What is this logic saying?
			// What does "expectEmptySlot" really mean?
			val existingMSR_opt: Option[MadeSpatRecBase] = if (toMake.expectEmptySlot) {
				val previousMadeSpatRec_opt = myMadeSpatRecsByID.get(assignedID)

				if (previousMadeSpatRec_opt.isDefined) {
					warn2("Found existing shape-spat at oldShape={}, so IGNORING newShape={}",
						previousMadeSpatRec_opt, toMake)
					previousMadeSpatRec_opt
				} else None
			} else None

			existingMSR_opt.getOrElse({

				val madeSpatRec: MadeSpatRecBase = {
					toMake match {
						case cgn: VWSCR_CamGuideNode => {
							new CamGuideMadeSpatRec(madeSpat.asInstanceOf[JmeNode], assignedID, cgn)
						}
						case otherN => {
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

	protected def unregisterSpat(shapeID: Ident): Unit = {
		myMadeSpatRecsByID.remove(shapeID)
	}

	// protected def findParentNode() : Option[JmeNode]
	// Uses node at knownParentID if specified, else attaches to 2D or 3D "root" node.
	def attachToParent_onRendThrd(madeSpat: Spatial, toMake: VWShapeCreateRq): Unit = {
		val knownParentID_opt = toMake.getKnownParentID_opt
		attachToParent_onRendThrd(madeSpat, toMake.getKnownParentID_opt, toMake.inFlatSpace)

	}

	private def attachToParent_onRendThrd(spatToAttach: Spatial, knownParentID_opt: Option[Ident], inFlatSpc: Boolean): Unit = {
		val knownParentNode_opt: Option[JmeNode] = if (knownParentID_opt.isDefined) {
			val kparid = knownParentID_opt.get
			val knownParentMadeRec_opt = myMadeSpatRecsByID.get(kparid)
			if (knownParentMadeRec_opt.isDefined) {
				val pNode: JmeNode = knownParentMadeRec_opt.get.getMainSpat.asInstanceOf[JmeNode]
				info1("Found known parentNode: {}", pNode)
				Option(pNode)
			} else {
				warn2("Could not find expected parentNode at ID={}, for child={}", kparid, spatToAttach)
				// TODO: Probably we should force failure in this case, not attach to default
				None
			}
		} else None

		val parentToUse_opt: Option[JmeNode] = knownParentNode_opt.orElse(
			if (inFlatSpc) {
				Option(myTopFlatNode)
			} else {
				Option(myTopDeepNode)
			}
		)

		info2("Using parentNode={} for childSpatial={}", parentToUse_opt, spatToAttach)
		val dooda = parentToUse_opt.map(_.attachChild(spatToAttach))
	}

	def reattachShapeToParent_onRendThrd(attchRq: VWShapeAttachRq): Unit = {
		val msr: MadeSpatRecBase = findMadeSpatRec(attchRq.knownID).get
		attachToParent_onRendThrd(msr.getMainSpat, attchRq.knownParentID_opt, false)
		msr.syncAfterAttach_onRendThrd
	}

	def attachBigGrid_onRendThrd(): Unit = {
		makeBigGridAndAttach_onRendThrd(getRRC, myTopDeepNode)
	}


	// Does this need to go through Brush, so we can set renderBucket and CullHint propertly?
	// (If alpha-transparency is not working reliably, that would be an explanation).
	def applyMatToSubtree(topShapeID: Ident, mat: Material): Unit = {
		val msr: MadeSpatRecBase = findMadeSpatRec(topShapeID).get
		val spat = msr.getMainSpat
		spat.setMaterial(mat) // Javadoc says this should propagate to sub-geoms.
		// http://javadoc.jmonkeyengine.org/com/jme3/scene/Spatial.html#setMaterial-com.jme3.material.Material-
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
			val doDeferAtch = true
			val unusedResult : MadeSpatRecBase = makeAndPlace(vwsrq, doDeferAtch)
		}
		case vwsdrq : VWShapeDeleteRq => {
			val shapeID = vwsdrq.shapeID
			val func : Function0[Unit] = () => {
				detachFromParent_onRendThrd(shapeID)
				unregisterSpat(shapeID)
			}
			enqueueJmeCallable(myRRC, func)

		}
		case manipRq : VWShapeManipRq => {
			val shapeID : Ident = manipRq.getTgtShapeID
			val madeSpatRec_opt : Option[MadeSpatRecBase] = findMadeSpatRec(shapeID)
			val manipDesc = manipRq.getManipDesc
			val func : Function0[Unit] = () => {
				info1("Finally applying manip for VWShapeManipRq={}", manipRq)
				madeSpatRec_opt.get.applyManipDesc(manipDesc, this, manipRq.getStatusHandler_opt)
			}
			enqueueJmeCallable(myRRC, func)
		}
		case matChgRq : VWApplyMatToAll => {
			val topShapeID : Ident = matChgRq.getTopShapeID
			val matDesc : VWMatDesc = matChgRq.getMatDesc
			val mat = myShapeMaker.getBestMat(matDesc)
			applyMatToSubtree(topShapeID, mat)
		}
	}
}


