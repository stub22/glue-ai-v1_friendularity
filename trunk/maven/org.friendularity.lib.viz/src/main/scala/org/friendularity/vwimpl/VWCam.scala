package org.friendularity.vwimpl

import com.jme3.math.{ColorRGBA, Vector2f, Vector3f}
import com.jme3.renderer.Camera
import com.jme3.scene.control.CameraControl
import com.jme3.scene.{Node => JmeNode, CameraNode}
import org.appdapter.core.name.Ident
import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.render.app.entity.CameraBinding
import org.cogchar.render.opengl.optic.CameraMgr
import org.cogchar.render.sys.registry.RenderRegistryClient
import org.cogchar.render.sys.task.Queuer
import org.cogchar.render.trial.PointerCone
import org.friendularity.cpmsg.CPMsgTeller
import org.friendularity.field.{TypedFieldDataLeafImpl, MediumFieldDataBag, VWTestFieldIdents, ItemFieldSpecDirectImpl, ItemFieldData, ReportFilteringPolicy, MonitoredSpaceImpl}
import org.friendularity.vwmsg.{ShapeManipRqImpl, AbruptManipAbsFullImpl, TransformParams3D, Transform3D, ViewportDesc, CamState3D, VWSCR_ExistingNode, VWBindCamNodeRq, VWModifyCamStateRq, VWCreateCamAndViewportRq}

/**
  * Created by Stub22 on 7/17/2016.
  */
case class VWCamSummary(camID : Ident, posLoc: Vector3f, pointDir: Vector3f, upDir: Vector3f,
						viewLowerLeft : Vector2f, viewTopRight : Vector2f) {
}
trait VWCamLogic extends VarargsLogging with IdentHlp with MonitoredSpaceImpl {
	// We rely on the Cogchar cam-registry to keep track of cams by ID.

	def getStageCtx : VWStageCtx
	lazy val myCamMgr : CameraMgr = getStageCtx.getRRC.getOpticCameraFacade(null);

	def makeCam_rendThrd(mcavRq : VWCreateCamAndViewportRq) : Unit = {
		info1("Making cam for rq: {}", mcavRq)
		val cbind : CameraBinding = myCamMgr.findOrMakeCameraBinding(mcavRq.camID) // makes the JME Camera object
		applyCamState_anyThrd(cbind, mcavRq.initState)
		applyViewportDesc_rendThrd(cbind, mcavRq.initVP)
		// "attach_" is Designed to only be called once for each cam/binding, it appears.
		cbind.attachViewPort(getStageCtx.getRRC)
		info1("Cam {} is now attached", mcavRq.camID)
	}

	def updateCamState_rendThrd(mcRq : VWModifyCamStateRq) : Unit = {
		val cbind : CameraBinding = myCamMgr.findOrMakeCameraBinding(mcRq.camID)
		info2("Found cbind={}, applying rq={}", cbind, mcRq)
		mcRq.updState_opt.map(applyCamState_anyThrd(cbind, _))
		mcRq.updVP_opt.map(applyViewportDesc_rendThrd(cbind, _))

	}
	private def positionGuideWhereCamIsNow(spcTeller : CPMsgTeller, guideNodeID : Ident, cam : Camera) : Unit = {
		// Pull the current cam xform so we can copy it as pos for the guide node.
		val xtractor = new CamXformXtractor {}
		val camCurrXform = xtractor.pullXformForCam(cam)
		val abruptManip = new AbruptManipAbsFullImpl(camCurrXform)
		val statusTlr_opt = None
		val guideManipMsg = new ShapeManipRqImpl(guideNodeID, abruptManip, statusTlr_opt)
		spcTeller.tellCPMsg(guideManipMsg)
	}
	def processBindCamNode(bcnRq : VWBindCamNodeRq) : Unit = {
		val cbind : CameraBinding = myCamMgr.findOrMakeCameraBinding(bcnRq.camID)
		val camNodeName = makeStampyRandyString("camNode_", "")
		val cam = cbind.getCamera

		positionGuideWhereCamIsNow(bcnRq.spaceTeller, bcnRq.spaceNodeID, cam)

		val camNode = new CameraNode(camNodeName, cam)

		if (bcnRq.flag_attachVisibleMarker) {
			val camMarkerNode: JmeNode = makePointerCone(getStageCtx.getRRC, "markerConeFor_" + camNodeName)
			camNode.attachChild(camMarkerNode)
		}

		camNode.setControlDir(CameraControl.ControlDirection.SpatialToCamera)

		val camNodeShapeID = makeStampyRandyIdentAnon()

		// Use the supplied nodeID from the request message (i.e. the "guide" node) as the *parent* of our newly created
		// camera-driver node.
		val registerCamNodeAsShape = new VWSCR_ExistingNode(camNode, camNodeShapeID, Option(bcnRq.spaceNodeID))

		// Unclear exactly how previous state of camera is affected/replaced by the node attachment.
		// Need more debug output.
		bcnRq.spaceTeller.tellCPMsg(registerCamNodeAsShape)
	}
	def makePointerCone(rrc : RenderRegistryClient, nameSuffix : String) : JmeNode = {
		val pc: PointerCone = new PointerCone(nameSuffix)
		pc.setup(rrc)
		pc.setTextPositionFraction(0.75f)
		pc.getAssemblyNode
	}
	def applyCamState_anyThrd(cbind : CameraBinding, camState : CamState3D) : Unit = {
		cbind.setWorldPos(camState.getPos)
		cbind.setPointDir(camState.getPointDir)
		cbind.applyInVWorld(Queuer.QueueingStyle.QUEUE_AND_RETURN)
	}
	def applyViewportDesc_rendThrd(cbind : CameraBinding, vpDesc : ViewportDesc): Unit = {
		//	val camID = cbind.getIdent()
		vpDesc.myBGColor_opt.map(cbind.setViewPortColor_rendThrd(_))  		// getViewPort
		val jmeCam = cbind.getCamera
		applyViewRectToCamState_rendThrd(jmeCam, vpDesc)
	}
	private def applyViewRectToCamState_rendThrd(cam : Camera, vpd : ViewportDesc): Unit = {
		cam.setViewPort(vpd.myX1_left, vpd.myX2_right, vpd.myY1_bot, vpd.myY2_top)
	}

	def setViewportBackroundColor_rendThrd (camID : Ident, bgColor : ColorRGBA): Unit = {
		val cbind : CameraBinding = myCamMgr.findOrMakeCameraBinding(camID)
		// setViewPortColor_rendThrd
	}
	override def makeInitialReportFields(chanID : Ident, filteringPolicy: ReportFilteringPolicy) : List[ItemFieldData] = {
		makeFullDumpToFields
	}
	override def makeTickUpdateReportFields(chanID : Ident, filteringPolicy: ReportFilteringPolicy) : List[ItemFieldData] = {
		makeFullDumpToFields
	}
	import collection.JavaConverters._
	import scala.collection.mutable.{HashMap => MutableHashMap}
	private def makeFullDumpToFields(): List[ItemFieldData] = {
		val allBindings : Iterable[CameraBinding] = myCamMgr.getAllCameraBindings.asScala

		val fieldCollsByItem = new MutableHashMap[Ident,Traversable[ItemFieldData]]
		// TODO:  Cache at least the fieldSpecs for reuse across updates
		// Also find a way to treat Vector as a single leaf value
		for (cb <- allBindings) {
			val id = cb.getIdent
			val cam = cb.getCamera
			// Note these can accept mutable buffer inputs
			val camPos = cam.getLocation
			val camDir = cam.getDirection
			val upDir = cam.getUp
			val leftDir = cam.getLeft
			val vpLeftBot = new Vector2f(cam.getViewPortLeft, cam.getViewPortBottom)
			val vpRightTop = new Vector2f(cam.getViewPortRight, cam.getViewPortTop)
			val frustLeftBotNear = new Vector3f(cam.getFrustumLeft, cam.getFrustumBottom, cam.getFrustumNear)
			val camSummary = new VWCamSummary(id, camPos, camDir, upDir, vpLeftBot, vpRightTop)

			val summaryFieldAddr = new ItemFieldSpecDirectImpl(id, VWTestFieldIdents.PROP_hasSummary)
			val summaryLeaf = new TypedFieldDataLeafImpl[VWCamSummary](summaryFieldAddr, camSummary)
			fieldCollsByItem.put(id, List(summaryLeaf))
		}
		val camsFieldAddr = new ItemFieldSpecDirectImpl(VWTestFieldIdents.ROOT_ITEM_CAMERAS, VWTestFieldIdents.PROP_hasCamera)

		val bag = new MediumFieldDataBag(camsFieldAddr, fieldCollsByItem.toMap)
		List(bag)
	}

}
trait CamXformXtractor extends VarargsLogging {
	def pullXformForCam(cam : Camera) : Transform3D = {
		val locPos = cam.getLocation
		val locRot = cam.getRotation // compare getDirection
		val locScl = Vector3f.UNIT_XYZ

		val xform3D = new TransformParams3D(locPos, locRot, locScl)
		info1("Pulled camXform={}", xform3D)
		xform3D
	}
}

trait SyncsToCam extends Movable with Locatable with CamXformXtractor {
	// This works, but unclear how long this state remains good after attachment of cam to a parent.
	// Probably gets wiped out as soon as we return from the render callback, eh?

	private def getGrandchildCamXform: Transform3D = {
		val guideNode : JmeNode = getMainSpat.asInstanceOf[JmeNode]
		val camNode = guideNode.getChild(0).asInstanceOf[CameraNode]
		val cam : Camera = camNode.getCamera
		info2("Pulling current transform of camera={} within camNode={}", cam, camNode)
		pullXformForCam(cam)
	}
	protected def syncGuideToCam_rendThrd(): Unit = {

		val xform = getGrandchildCamXform
		applyTransform_partial_runThrd(xform)

	}
}

