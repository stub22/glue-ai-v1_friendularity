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
package org.friendularity.vw.cli.cam

import java.lang.{Float => JFloat, Integer => JInt, Long => JLong}

import org.appdapter.core.name.Ident
import org.appdapter.fancy.log.VarargsLogging
import org.friendularity.cpmsg.{CPMsgTeller, CPStrongTeller}
import org.friendularity.util.IdentHlp

import org.friendularity.vw.mprt.manip.{CamState3D, MakesManipDesc, ManipStatusMsg, MaybeTransform3D}
import org.friendularity.vw.msg.shp.deep.{ShapeManipRqImpl, VWSCR_CamGuideNode, VWSCR_Node}
import org.friendularity.vw.msg.stg.{VWBindCamNodeRq, VWCreateCamAndViewportRq, VWModifyCamStateRq, ViewportDesc}

/**
  * Created by Stub22 on 8/13/2016.
  */
trait OuterCamHelp extends MakesManipDesc with IdentHlp with VarargsLogging {

	def makeAndBindExtraCam(stageTeller : CPMsgTeller, spcTeller : CPMsgTeller, camShortLabel : String,
							initCamState: CamState3D, initVP : ViewportDesc) : Ident = {
		val camID : Ident = makeStampyRandyIdent(camShortLabel + "_intrnCam")
		val camGuideNodeID: Ident = makeStampyRandyIdent(camShortLabel + "_camGuide")
		makeAndBindExtraCam(stageTeller, spcTeller, camID, camGuideNodeID, initCamState, initVP)
		camGuideNodeID
	}
	def makeAndBindExtraCam(stageTeller : CPMsgTeller, spcTeller : CPMsgTeller,
			camID : Ident,	camGuideNodeID : Ident, initCamState: CamState3D, initVP : ViewportDesc) : Unit = {

		info1("Requesting cam at ID={}", camID)
		val makeCamRq = new VWCreateCamAndViewportRq(camID, initCamState, initVP)
		stageTeller.tellCPMsg(makeCamRq)

		info1("Requesting cam(xtra)-guide node at ID={}", camGuideNodeID)
		val makeGuideNodeRQ = new VWSCR_Node(camGuideNodeID, None)
		spcTeller.tellCPMsg(makeGuideNodeRQ)

		val flag_guideIsParent = true
		info2("Binding xtra cam at ID={} to cam-guide node at ID={}", camID, camGuideNodeID)

		val flag_attachVisibleMarker = true
		val camGuideBindRq = new VWBindCamNodeRq(camID, flag_guideIsParent, spcTeller, camGuideNodeID, flag_attachVisibleMarker)
		stageTeller.tellCPMsg(camGuideBindRq)
	}
	def bindKnownCam(stageTeller : CPMsgTeller, spcTeller : CPMsgTeller,
							camID : Ident,	camGuideNodeID : Ident, flag_attachVisibleMarker : Boolean) : Unit = {
		// TODO:  Move this first step into the stage teller so that camGuide can sync to cam in predictable way
		info1("Requesting cam(known)-guide node at ID={}", camGuideNodeID)
		val makeGuideNodeRQ = new VWSCR_CamGuideNode(camGuideNodeID, None)
		spcTeller.tellCPMsg(makeGuideNodeRQ)

		val flag_guideIsParent = true
		info2("Binding known cam at ID={} to cam-guide node at ID={}", camID, camGuideNodeID)

		// Cam guide node will become the parent of the camera-Node
		val camGuideBindRq = new VWBindCamNodeRq(camID, flag_guideIsParent, spcTeller, camGuideNodeID, flag_attachVisibleMarker)
		stageTeller.tellCPMsg(camGuideBindRq)
	}
	// Returns ID of the statusHandler
	def sendGuidedCamMoveRq(spcTeller : CPMsgTeller, xtraCamGuideNodeID : Ident, mayXform : MaybeTransform3D,
							durSec_opt : Option[JFloat], statusTlr_opt : Option[CPStrongTeller[ManipStatusMsg]] ) : Option[Ident] = {
		val forceToFullXform = false // "Partial" approach is preferred as of 2016-Nov, see RVWS-49 and RVWS-57.
		val manipGuts = makeManipGuts(mayXform, durSec_opt, forceToFullXform)
		val guideManipMsg = new ShapeManipRqImpl(xtraCamGuideNodeID, manipGuts, statusTlr_opt)
		val statusHandlerID_opt = guideManipMsg.getStatusHandler_opt.map(_.getHandleID) //  manipGuts.getManipID
		info2("Sending guided cam manip msg with statusHandlerID_opt={} to spcTeller: {}", statusHandlerID_opt, guideManipMsg)
		spcTeller.tellCPMsg(guideManipMsg)
		statusHandlerID_opt
	}

	def sendCamStateModifyRq(stgTeller : CPMsgTeller, camID : Ident, updState_opt : Option[CamState3D], updVP_opt: Option[ViewportDesc]) : Unit = {
		val msg = new VWModifyCamStateRq(camID, updState_opt, updVP_opt)
		stgTeller.tellCPMsg(msg)
	}

}
