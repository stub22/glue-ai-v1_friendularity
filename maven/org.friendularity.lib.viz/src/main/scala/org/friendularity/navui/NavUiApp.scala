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

import akka.actor.{ActorRef, ActorSystem, ActorRefFactory}
import org.appdapter.core.name.{FreeIdent, Ident}
import org.appdapter.fancy.log.VarargsLogging
import org.appdapter.fancy.rclient.EnhancedLocalRepoClient
import org.cogchar.api.humanoid.HumanoidFigureConfig
import org.cogchar.bind.mio.robot.svc.ModelBlendingRobotServiceContext
import org.friendularity.cpmsg.{ActorRefCPMsgTeller, CPStrongTeller, CPMsgTeller, CPumpMsg}

import org.friendularity.dull.SpecialAppPumpSpace
import org.friendularity.field.ScheduleHelper
import org.friendularity.respire._
import org.friendularity.vwimpl.{VWorldActorFactoryFuncs, LegacyBodyLoader_Stateless}
import org.friendularity.vwmsg.{VWorldPublicTellers, VWBodyRq, VWBodyMakeRq, VWBodyLifeRq, VWARM_FindPublicTellers, VWSetupRq_Lnch, VWSetupRq_Conf, VWARM_GreetFromPumpAdmin, VWBodyNotice}
import org.osgi.framework.BundleContext

/**
  * Created by Stub22 on 6/8/2016.
  */
trait NavUiAppSvc extends VarargsLogging {

	def postPatientCharCreateRq(dualBodyID : Ident, fullHumaCfg : HumanoidFigureConfig,
								mbrsc_opt : Option[ModelBlendingRobotServiceContext],
								answerTeller : CPStrongTeller[VWBodyNotice]) : Unit

	def makeExoBodyUserTeller_withTicks(akkaSys : ActorSystem, ebuActorName : String, userLogic : ExoBodyUserLogic) : CPStrongTeller[VWBodyNotice] = {
		val parentARF : ActorRefFactory = akkaSys
		val ebuActor : ActorRef = ExoActorFactory.makeExoBodyUserActor(parentARF, ebuActorName, userLogic)
		val ebuTeller : CPStrongTeller[VWBodyNotice] = new ActorRefCPMsgTeller[VWBodyNotice](ebuActor)
		// Let the user logic create the TickItem = callback func and schedule params
		val regTickItem = userLogic.makeRegularTickItem()
		regTickItem.addToSchedForSys(akkaSys, ebuActor, ebuActor)
		ebuTeller
	}
	def UNUSED_scheduleCallback_UNUSED(akkaSys : ActorSystem, tgtActor : ActorRef, tickMsg : CPumpMsg, phaseMillis : Integer,
									   periodMillis : Integer, schedHelper : ScheduleHelper): Unit = {
		val schedItem = schedHelper.makeSchedItemRepeating(tickMsg, phaseMillis, periodMillis)
		schedItem.addToSchedForSys(akkaSys, tgtActor, tgtActor)
	}

	def makeFunUserLogic(): ExoBodyUserLogic = {
		val btc = new BodyTestClient {}
		val userLogic: ExoBodyUserLogic = btc.makeMoveTestLogic()
		userLogic
	}

	val sinbadBodyID : Ident = new FreeIdent("urn:ftd:cogchar.org:2012:runtime#char_sinbad_88")
	val sinbadHmdGraphID: Ident = new FreeIdent("urn:ftd:cogchar.org:2012:runtime#hmd_sheet_22")
	val sinbadBonyGraphID: Ident = new FreeIdent("urn:ftd:cogchar.org:2012:runtime#bony_sheet_sinbad")

	// Includes MechIO anim connections, requires bundleCtx.
	// TODO:  If we were sending to an actor that knew how to discover the MechIOBody connection,
	// possibly by waiting for a lifecycle update, then this impl could be same as the "Standy" method below.
	def requestSemiLegacyBodyConn_OSGi_Sinbad(bundleCtx: BundleContext, akkaSys: ActorSystem,
					 legacyELRC: EnhancedLocalRepoClient, exoBodyUserLogic : ExoBodyUserLogic): Unit = {
		requestSemiLegacyBodyConn_OSGi(bundleCtx, akkaSys, legacyELRC, sinbadBodyID,
					sinbadHmdGraphID, sinbadBonyGraphID, exoBodyUserLogic)
	}
	def requestSemiLegacyBodyConn_OSGi(bundleCtx: BundleContext, akkaSys: ActorSystem,
						legacyELRC: EnhancedLocalRepoClient, charBodyID : Ident,
						hmdGraphID : Ident, bonyGraphID : Ident,
						exoBodyUserLogic : ExoBodyUserLogic): Unit = {

		val legBodyLdr = new LegacyBodyLoader_Stateless
		val fullHumaCfg : HumanoidFigureConfig = legBodyLdr.loadFullHumaConfig_SemiLegacy(legacyELRC, charBodyID, hmdGraphID, bonyGraphID)
		val mbrsc: ModelBlendingRobotServiceContext = legBodyLdr.connectMechIOBody(legacyELRC, bundleCtx, fullHumaCfg, bonyGraphID)

		// Before we create the body itself, create an external client actor, with ability to
		// talk to VW body, and also to receive schedule ticks
		val noticerName = "bodyNoticer_" + charBodyID.getLocalName
		val bodyNoticer : CPStrongTeller[VWBodyNotice] = makeExoBodyUserTeller_withTicks(akkaSys, noticerName, exoBodyUserLogic)

		// Now we've done all the "outer" setup that requires assumptions, and we can
		// send off a tidy async request to the v-world actors, requesting them to
		// instantiate the avatar body and send back a notice when done, to our bodyNoticer.
		// THEN our bodyNoticer can send more requests do any additional manipulation on the body
		// such as move its v-world position and orientation, attach a camera, launch an animation.

		postPatientCharCreateRq(charBodyID, fullHumaCfg, Option(mbrsc), bodyNoticer)

	}
	// Creates a posable VW character, but does not ask for or assume any MechIO (or other OSGi) infrastructure.
	def requestStandySemiLegacyBody_Sinbad(akkaSys: ActorSystem,
										   legacyELRC: EnhancedLocalRepoClient,
										   exoBodyUserLogic : ExoBodyUserLogic): Unit = {
		val legBodyLdr = new LegacyBodyLoader_Stateless
		val fullHumaCfg : HumanoidFigureConfig = legBodyLdr.loadFullHumaConfig_SemiLegacy(legacyELRC, sinbadBodyID, sinbadHmdGraphID, sinbadBonyGraphID)

		// Before we create the body itself, create an external client actor, with ability to
		// talk to VW body, and also to receive schedule ticks
		val bodyNoticer : CPStrongTeller[VWBodyNotice] = makeExoBodyUserTeller_withTicks(akkaSys, "sinbad_standy_body_user", exoBodyUserLogic)
		postPatientCharCreateRq(sinbadBodyID, fullHumaCfg, None, bodyNoticer)


	}
}

trait NavPumpSpaceOwner extends VarargsLogging {
	protected def getAkkaSys : ActorSystem
	lazy private val myPumpSpace = new SpecialAppPumpSpace(getAkkaSys)
	lazy private val standPumpTestCtxName = NavUiTestPublicNames.cpumpName
	lazy private val standPumpCtxActorRef : ActorRef = myPumpSpace.findTopActorRef(standPumpTestCtxName)
	lazy private val standPumpAdminTeller = new ActorRefCPMsgTeller(standPumpCtxActorRef)

	protected def sendGreetMsgs_Async(vwBossTeller : CPMsgTeller) : Unit = {
		// We send a currently-non-essential administrative howdy to get the game rollin
		val hpatMsg = new VWARM_GreetFromPumpAdmin(standPumpAdminTeller)
		info2("Sending greeting msg={} to VWBossTeller : {}", hpatMsg, vwBossTeller)
		vwBossTeller.tellCPMsg(hpatMsg)
	}

}
// "App" here means FriendU app, not a JME3 "app".  (This instance is several layers further out)
// The latter is made during launchSimRenderSpace in VWCore.scala.

class NavUiAppImpl(myAkkaSys : ActorSystem) extends NavUiAppSvc with NavPumpSpaceOwner {

	override protected def getAkkaSys : ActorSystem = myAkkaSys

	lazy private val myVWBossAR: ActorRef = VWorldActorFactoryFuncs.makeVWorldBoss(myAkkaSys, "vworldBoss_818")
	lazy private val myVWBossTeller = new ActorRefCPMsgTeller(myVWBossAR)

	// Jobby approach to actor launch is used here for our outer actors, experimentally.
	lazy private val goodyTestSenderLogic = new PatientSender_GoodyTest {}
	lazy private val goodyTestSenderTrigTeller  = OuterJobbyLogic_MasterFactory.makeOoLogicAndTeller(goodyTestSenderLogic, myAkkaSys, "goodyTstSndr")

	lazy private val charAdmForwarderLogic = new PatientForwarder_CharAdminTest {}
	lazy private val charAdmSenderTrigTeller  = OuterJobbyLogic_MasterFactory.makeOoLogicAndTeller(charAdmForwarderLogic, myAkkaSys, "charAdmForwarder")

	lazy private val bonusStagingLogic = new PatientSender_BonusStaging {}
	lazy private val bonusStagingTrigTeller  = OuterJobbyLogic_MasterFactory.makeOoLogicAndTeller(bonusStagingLogic, myAkkaSys, "bonusStagingRequester")

	lazy private val statusTickPumpLogic = new OuterAppPumpSetupLogic {
		override protected def getAkkaSystem : ActorSystem = getAkkaSys
	}

	lazy private val statusTickTrigTeller  = OuterJobbyLogic_MasterFactory.makeOoLogicAndTeller(statusTickPumpLogic, myAkkaSys, "statusTickPumpSetup")


	// Desired effect of these messages is to launch a running OpenGL v-world, ready for characters and other content
	// to be inserted into it.  Those facilities are available via actors defined in PubTeller replies sent to the
	// postInitWaiters.
	def sendSetupMsgs_Async {

		sendGreetMsgs_Async(myVWBossTeller)  // No concrete effect as of 2016-06-16

		// As of 2016-06-16 "Conf" is disabled, and we proceed directly to launch.
		// When active, this conf step causes a duplicate copy of legacy config repo to be
		// loaded, which we don't have any actual use for presently.   Under
		// OSGi there is an outer copy of that same config repo, used for launching
		// the avatar bodies.
		// sendVWSetup_Conf()

		sendVWSetup_Lnch() // First and only call that really makes async launch happen, as of 206-06-17

		registerPostInitWaiters() // Setup listeners to do more stuff at appropriate times, as VWorld init completes.
	}
	def sendVWSetup_Conf() : Unit = {
		val msg = new VWSetupRq_Conf
		myVWBossTeller.tellCPMsg(msg)
	}

	def sendVWSetup_Lnch() : Unit = {
		val msg = new VWSetupRq_Lnch
		myVWBossTeller.tellCPMsg(msg)
	}
	def registerPostInitWaiters() : Unit = {
		// Each of these the results of happy startup, to trigger further ops.
		// The VWPTRendezvous logic makes sure that each such waiter gets notified regardless of message order,
		// so it is OK to send them after init is already complete (although usually it won't be).
		val goodyTstRegMsg = new VWARM_FindPublicTellers(goodyTestSenderTrigTeller)
		debug2("Sending goody-listener--reg={} to VWBossTeller : {}", goodyTstRegMsg, myVWBossTeller)
		myVWBossTeller.tellCPMsg(goodyTstRegMsg)

		val charAdmRegMsg = new VWARM_FindPublicTellers(charAdmSenderTrigTeller)
		debug2("Sending char-admin-listener-reg={} to VWBossTeller : {}", charAdmRegMsg, myVWBossTeller)
		myVWBossTeller.tellCPMsg(charAdmRegMsg)

		val bonusStageRegMsg = new VWARM_FindPublicTellers(bonusStagingTrigTeller)
		debug2("Sending bonusStage-listener-reg={} to VWBossTeller : {}", bonusStageRegMsg, myVWBossTeller)
		myVWBossTeller.tellCPMsg(bonusStageRegMsg)

		val tickPumpRegMsg = new VWARM_FindPublicTellers(statusTickTrigTeller)
		debug2("Sending tickPump-listener-reg={} to VWBossTeller : {}", tickPumpRegMsg, myVWBossTeller)
		myVWBossTeller.tellCPMsg(tickPumpRegMsg)

	}
	// Can't so directly send this yet as  actor msg.  There is a kind of implicit rendezvous going on here
	// between  the optional MechIO connection, the legacy-repo-based humaConfig, and the launched VWorld actors.
	def appendCharAdmRq(chrAdmRq : VWBodyLifeRq) : Unit = charAdmForwarderLogic.appendInboundRq(chrAdmRq)

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