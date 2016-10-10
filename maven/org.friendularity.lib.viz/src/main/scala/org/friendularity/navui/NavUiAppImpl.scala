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

import akka.actor.ActorSystem
import java.awt.Image
import org.appdapter.core.name.Ident
import org.cogchar.api.humanoid.HumanoidFigureConfig
import org.cogchar.bind.mio.robot.svc.ModelBlendingRobotServiceContext
import org.friendularity.cpmsg.CPStrongTeller
import org.friendularity.qpc.OffersVWorldServer
import org.friendularity.respire.DetachedGST
import org.friendularity.vwimpl.MakesVWBoss
import org.friendularity.vwmsg.{VWBodyMakeRq, VWBodyNotice, VWSetupRq_Lnch, VWSetupRq_Conf, VWSetSwingCanvasBranding}

/**
  * Created by Stub22 on 8/8/2016.
  */
class NavUiAppImpl(myAkkaSys : ActorSystem) extends NavUiAppSvc with NavAppCloser with NavPumpSpaceOwner
			with AppServiceHandleGroup with MakesVWBoss with OffersVWorldServer  {

	override protected def getAkkaSys : ActorSystem = myAkkaSys
	override protected def findAppQpidSvcOffering_opt : Option[OffersVWorldServer] = Some(this)

	// Desired effect of these messages is to launch a running OpenGL v-world, ready for characters and other content
	// to be inserted into it.  Those facilities are available via actors defined in PubTeller replies sent to the
	// postInitWaiters.
	def sendSetupMsgs_Async(wrapWithSwing : Boolean) {
		val vbt = getVWBossTeller // triggers creation of Boss Actor + teller
		sendGreetMsgs_Async(vbt)  // Validates actor messaging, otherwise no significant effect as of 2016-06-16

		// As of 2016-06-16 an earlier experiment with "VWSetup_Conf" is disabled, and we proceed directly to launch.
		// When active, this conf step causes a duplicate copy of legacy config repo to be
		// loaded, which we don't have any actual use for presently.   Under
		// OSGi there is an outer copy of that same config repo, used for launching
		// the avatar bodies.
		// sendVWSetup_Conf()

		sendVWSetup_Lnch(wrapWithSwing) // First and only call that really makes async launch happen, as of 2016-06-17

		registerPostInitWaiters(vbt) // Setup listeners to do more stuff at appropriate times, as VWorld init completes.

	}

	def sendVWSetup_Conf_IsUnused() : Unit = {
		val msg = new VWSetupRq_Conf
		getVWBossTeller.tellCPMsg(msg)
	}

	def sendVWSetup_Lnch(wrapWithSwing : Boolean) : Unit = {
		val fixmeClzrNonSerial : NavAppCloser = this
		val msg = new VWSetupRq_Lnch(wrapWithSwing, fixmeClzrNonSerial)
		getVWBossTeller.tellCPMsg(msg)
	}
    
    def sendVWSetSwingCanvasBranding(canvasTitle: String,  canvasIconImage : Image) : Unit = {
		val msg = new VWSetSwingCanvasBranding(canvasTitle, canvasIconImage)
		getVWBossTeller.tellCPMsg(msg)
	}

	override def postPatientCharCreateRq(dualBodyID : Ident, fullHumaCfg : HumanoidFigureConfig,
										 mbrsc_opt : Option[ModelBlendingRobotServiceContext], answerTeller : CPStrongTeller[VWBodyNotice]) : Unit = {
		val ccrq = new VWBodyMakeRq(dualBodyID, fullHumaCfg, mbrsc_opt, answerTeller)
		appendCharAdmRq(ccrq)
	}
	def testDetachedGS : Unit = {
		val dgst = new DetachedGST{}
		dgst.gridSpaceTest
	}
}