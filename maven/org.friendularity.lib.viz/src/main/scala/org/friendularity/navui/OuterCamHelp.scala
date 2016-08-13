package org.friendularity.navui

import org.appdapter.core.name.Ident
import org.appdapter.fancy.log.VarargsLogging
import org.friendularity.cpmsg.CPMsgTeller
import org.friendularity.vwimpl.IdentHlp
import org.friendularity.vwmsg.{VWBindCamNodeRq, VWSCR_Node, VWCreateCamAndViewportRq, ViewportDesc, CamState3D}

/**
  * Created by Owner on 8/13/2016.
  */
trait OuterCamHelp extends IdentHlp with VarargsLogging {
	def makeAndBindExtraCam(stageTeller : CPMsgTeller, spcTeller : CPMsgTeller, camShortLabel : String,
							initCamState: CamState3D, initVP : ViewportDesc) : Ident = {

		val camID : Ident = makeStampyRandyIdent(camShortLabel + "_intrnCam")
		val makeCamRq = new VWCreateCamAndViewportRq(camID, initCamState, initVP)
		stageTeller.tellCPMsg(makeCamRq)

		val camGuideNodeID : Ident = makeStampyRandyIdent(camShortLabel + "_camGuide")
		info1("Requesting cam-guide node at ID={}", camGuideNodeID)
		val makeGuideNodeRQ = new VWSCR_Node(camGuideNodeID, None)
		spcTeller.tellCPMsg(makeGuideNodeRQ)

		val guideIsParent = true
		val camGuideBindRq = new VWBindCamNodeRq(camID, guideIsParent, spcTeller, camGuideNodeID)
		stageTeller.tellCPMsg(camGuideBindRq)
		camGuideNodeID
	}

}
