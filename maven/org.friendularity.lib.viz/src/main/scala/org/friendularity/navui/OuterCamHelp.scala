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
package org.friendularity.navui

import java.lang.{Integer => JInt, Long => JLong, Float => JFloat}


import com.jme3.math.{Quaternion, Vector3f}
import org.appdapter.core.name.Ident
import org.appdapter.fancy.log.VarargsLogging
import org.friendularity.cpmsg.CPMsgTeller
import org.friendularity.vwimpl.IdentHlp
import org.friendularity.vwmsg.{VWModifyCamStateRq, AbruptManipAbsImpl, ManipDesc, MakesTransform3D, Transform3D, MaybeTransform3D, ShapeManipRqImpl, SmooveManipEndingImpl, TransformParams3D, VWBindCamNodeRq, VWSCR_Node, VWCreateCamAndViewportRq, ViewportDesc, CamState3D}

/**
  * Created by Stub22 on 8/13/2016.
  */
trait OuterCamHelp extends MakesTransform3D with IdentHlp with VarargsLogging {

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

		info1("Requesting cam-guide node at ID={}", camGuideNodeID)
		val makeGuideNodeRQ = new VWSCR_Node(camGuideNodeID, None)
		spcTeller.tellCPMsg(makeGuideNodeRQ)

		val guideIsParent = true
		info2("Binding cam at ID={} to cam-guide node at ID={}", camGuideNodeID, camID)

		val camGuideBindRq = new VWBindCamNodeRq(camID, guideIsParent, spcTeller, camGuideNodeID)
		stageTeller.tellCPMsg(camGuideBindRq)
	}
	def sendXtraCamMoveRq(spcTeller : CPMsgTeller, xtraCamGuideNodeID : Ident, mayXform : MaybeTransform3D, durSec_opt : Option[JFloat]) : Unit = {
		val concXform : Transform3D = makeDefiniteXForm(mayXform)
		val manipGuts : ManipDesc = if (durSec_opt.isDefined) {
			new SmooveManipEndingImpl(concXform, durSec_opt.get)
		} else {
			new AbruptManipAbsImpl(concXform)
		}
		val guideManipMsg = new ShapeManipRqImpl(xtraCamGuideNodeID, manipGuts)
		spcTeller.tellCPMsg(guideManipMsg)
	}

	def sendCamStateModifyRq(stgTeller : CPMsgTeller, camID : Ident, updState_opt : Option[CamState3D], updVP_opt: Option[ViewportDesc]) : Unit = {
		val msg = new VWModifyCamStateRq(camID, updState_opt, updVP_opt)
		stgTeller.tellCPMsg(msg)
	}

}
