package org.friendularity.vw.impl.ta

import akka.actor.ActorRef
import com.jme3.math.Vector3f
import org.appdapter.core.name.{FreeIdent, Ident}
import org.cogchar.api.thing.ThingActionSpec
import org.cogchar.name.cinema.LightsCameraAN
import org.cogchar.render.app.entity.GoodyActionExtractor
import org.friendularity.infra.cpmsg.{ActorRefCPMsgTeller, CPMsgTeller}
import org.friendularity.infra.util.IdentHlp
import org.friendularity.vw.cli.cam.OuterCamHelp
import org.friendularity.vw.mprt.manip.{CamStateParams3D, CamState3D, MakesTransform3D, ManipStatusMsg, MaybeTransform3D}
import org.friendularity.vw.msg.pub.VWorldPublicTellers
import org.friendularity.vw.msg.shp.deep.{VWShapeAttachRq, VWShapeDetachRq}
import org.friendularity.vw.msg.stg.ViewportDesc

import java.lang.{Float => JFloat, Integer => JInt, Long => JLong}



/**
  * Created by Owner on 1/26/2017.
  */
trait DfltCamGuideMgr extends OuterCamHelp with TARqExtractorHelp with IdentHlp {
	protected def getVWPubTellers : VWorldPublicTellers

	private lazy val myDfltCamGuideID : Ident = makeStampyRandyIdent("dfltCamGuide")
	private var dfltCamIsBoundToGuide : Boolean = false

	private var dfltCamGuideIsBoundToRoot = false

	private var myLastMoveStatHandlerID_opt : Option[Ident] = None

	protected def getLastSendStatHandlerID_opt : Option[Ident] = myLastMoveStatHandlerID_opt

	protected def ensureDfltCamIsBoundToGuide(dfltCamID : Ident): Ident = {
		if (!dfltCamIsBoundToGuide) {
			val stageTeller : CPMsgTeller = getVWPubTellers.getStageTeller.get
			val spcTeller : CPMsgTeller = getVWPubTellers.getShaperTeller.get

			val flag_attachVisibleMarker = false

			bindKnownCam(stageTeller, spcTeller, dfltCamID, myDfltCamGuideID, flag_attachVisibleMarker)

			info0("Sleeping for 150 msec as clumsy insurance against race condition in setting up cam-guide")
			Thread.sleep(150)

			dfltCamIsBoundToGuide = true
			dfltCamGuideIsBoundToRoot = true
		} else {
			if (!dfltCamGuideIsBoundToRoot) {
				reattachDfltCamGuideToRoot()
			}
		}
		myDfltCamGuideID
	}
	protected def detachDfltCamGuideFromRoot(): Unit = {
		val guideNodeID = myDfltCamGuideID
		val spcTeller : CPMsgTeller = getVWPubTellers.getShaperTeller.get
		val detachRq = new VWShapeDetachRq(guideNodeID)
		spcTeller.tellCPMsg(detachRq)
		dfltCamGuideIsBoundToRoot = false
	}
	private def reattachDfltCamGuideToRoot(): Unit = {
		val guideNodeID = myDfltCamGuideID
		info1("Reattaching dflt-cam guide at ID={} to parent (root)", guideNodeID)
		val reattachGuideNodeRQ = new VWShapeAttachRq(guideNodeID, None)
		val spcTeller : CPMsgTeller = getVWPubTellers.getShaperTeller.get
		spcTeller.tellCPMsg(reattachGuideNodeRQ)

		dfltCamGuideIsBoundToRoot = true
	}
	protected def moveDfltCamViaGuide(ta : ThingActionSpec, gax: GoodyActionExtractor, whoDat : ActorRef,
									  flag_allowDur : Boolean) : Unit = {
		val camID = gax.getGoodyID
		val tvm = ta.getParamTVM

		// 		val stageTeller : CPMsgTeller = getVWPubTellers.getStageTeller.get

		// Possible minor race condition here, since cam-binding may not be completed in stage teller
		// before guided movement begins in space teller.  However, seems that the movement
		// is independent, and thus should catch-up anyway.
		val guideShapeID = ensureDfltCamIsBoundToGuide(camID)
		val maybeXform : MaybeTransform3D = extractXform(tvm, gax)
		val dur_opt : Option[JFloat] = if (flag_allowDur) extractDuration(tvm) else None

		val spcTeller : CPMsgTeller = getVWPubTellers.getShaperTeller.get

		val cmplStsTlr = new ActorRefCPMsgTeller[ManipStatusMsg](whoDat)
		val statusHandlerID_opt : Option[Ident] = sendGuidedCamMoveRq(spcTeller, guideShapeID, maybeXform, dur_opt, Some(cmplStsTlr))
		val statHandlerID = statusHandlerID_opt.get
		myLastMoveStatHandlerID_opt = statusHandlerID_opt
	}
	def receiveManipStatus (manipStat : ManipStatusMsg) : Unit = {
		info1("CamTARouterLogic received manipStat={}", manipStat)
		val handleID = manipStat.getHandleID
		val lastSentHandleID = myLastMoveStatHandlerID_opt.get
		if (lastSentHandleID.equals(handleID)) {
			info1("Ahoy - got completion for handlerID={}, so this is where we want to detach the main-cam guide node", handleID)
			detachDfltCamGuideFromRoot()
		} else {
			warn2("Ignoring completion for handleID={}, because it does not match our lastSentHandleID={}", handleID, lastSentHandleID)
		}
	}
}
trait CamTARouterLogic extends TARqExtractorHelp with MakesTransform3D with OuterCamHelp with DfltCamGuideMgr {


	def handleCameraTA(ta : ThingActionSpec, gax: GoodyActionExtractor, whoDat : ActorRef) : Unit = {
		val camGoodyID = gax.getGoodyID
		val dfltCamID = new FreeIdent(LightsCameraAN.URI_defaultCam);// CameraMgr.DEF_CAM_ID
		if (camGoodyID.equals(dfltCamID)) {
			handleDfltCameraManipTA(ta, gax, whoDat)
		} else {
			handleXtraCameraGuideTA(ta, gax, whoDat)
		}
	}
	private def handleDfltCameraManipTA(ta : ThingActionSpec, gax: GoodyActionExtractor, whoDat : ActorRef) : Unit = {
		val camID = gax.getGoodyID
		val tvm = ta.getParamTVM

		val stageTeller : CPMsgTeller = getVWPubTellers.getStageTeller.get
		val opKind = gax.getKind

		val useDirectMoves = false
		opKind match {

			case GoodyActionExtractor.Kind.CREATE => {
				// OPTIONAL:  Interpreted as request to bind guide shape
				val guideShapeID = ensureDfltCamIsBoundToGuide(camID)
			}

			case GoodyActionExtractor.Kind.MOVE => {
				if (useDirectMoves) {
					routeDirectCamMove(ta, gax)
				} else {
					moveDfltCamViaGuide(ta, gax, whoDat, true)
				}
			}
			case GoodyActionExtractor.Kind.SET => {
				// TODO:  Can be used to set the viewport
				moveDfltCamViaGuide(ta, gax, whoDat, false)
			}
			case otherX => {
				warn2("Got unexpected camera-direct op={}, full TA={}", opKind, ta)
			}
		}
	}

	private def routeDirectCamMove(ta : ThingActionSpec, gax: GoodyActionExtractor) : Unit = {

		val camID = gax.getGoodyID
		val tvm = ta.getParamTVM

		val stageTeller : CPMsgTeller = getVWPubTellers.getStageTeller.get

		info1("Routing cam-direct-move request based on TA={}", ta)

		val maybeXform: MaybeTransform3D = extractXform(tvm, gax)
		val dur_opt: Option[JFloat] = extractDuration(tvm)

		val worldPos = maybeXform.getPos // defaults to ZERO
		val worldPointQuat = maybeXform.getRotQuat // defaults to IDENTITY

		val negZDirVect =	new Vector3f(0, 0, -1)
		val worldPointDirVect = worldPointQuat.mult(negZDirVect)
		val updState_opt : Option[CamState3D] = Some(CamStateParams3D(worldPos, worldPointDirVect))
		val updVP_opt: Option[ViewportDesc] = None

		sendCamStateModifyRq(stageTeller, camID, updState_opt, updVP_opt)

	}
	def handleXtraCameraGuideTA(ta : ThingActionSpec, gax: GoodyActionExtractor, whoDat : ActorRef) : Unit = {
		// Resolve message cam-URI to paired shape ID, which is used for most camera movement control.
		// However, if we want to use ".lookAt" ...
		// OR, if we want to control a default camera (which does not have a parent shape ID)...

		val camGuideShapeID = gax.getGoodyID
		val tvm = ta.getParamTVM

		val stageTeller : CPMsgTeller = getVWPubTellers.getStageTeller.get
		val spcTeller : CPMsgTeller = getVWPubTellers.getShaperTeller.get
		val opKind = gax.getKind
		opKind match {
			case GoodyActionExtractor.Kind.CREATE => {
				val camShortLabel: String = "camMadeByTARouter"
				val initCamPos : Vector3f = Vector3f.ZERO
				val initPointDir : Vector3f = Vector3f.UNIT_Z // .negate()
				val initCamState: CamState3D = new CamStateParams3D(initCamPos, initPointDir)
				val (left, right, bot, top) = (0.7f, 0.95f, 0.75f, 0.95f)
				val bgColor_opt = extractColor(gax) // .orElse(Some(ColorRGBA.Blue))
				val initVP = new ViewportDesc(left, right, bot, top, bgColor_opt)
				val camID : Ident = makeStampyRandyIdent(camShortLabel + "_intrnCam")
				makeAndBindExtraCam(stageTeller, spcTeller, camID, camGuideShapeID, initCamState, initVP)
			}
			case GoodyActionExtractor.Kind.MOVE => {
				info1("Processing cam-move request: {}", ta)

				val maybeXform : MaybeTransform3D = extractXform(tvm, gax)
				val dur_opt : Option[JFloat] = extractDuration(tvm)

				sendGuidedCamMoveRq(spcTeller, camGuideShapeID, maybeXform, dur_opt, None)

			}
			case GoodyActionExtractor.Kind.SET => {
				info1("Processing cam-set request: {}", ta)
				val maybeXform : MaybeTransform3D = extractXform(tvm, gax)
				sendGuidedCamMoveRq(spcTeller, camGuideShapeID, maybeXform, Option.empty[JFloat], None)
			}
			case GoodyActionExtractor.Kind.DELETE => {

			}
		}
	}
}