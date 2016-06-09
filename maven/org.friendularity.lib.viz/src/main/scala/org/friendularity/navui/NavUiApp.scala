package org.friendularity.navui

import akka.actor.{ActorRef, ActorSystem, ActorRefFactory}
import org.appdapter.core.name.Ident
import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.api.humanoid.HumanoidFigureConfig
import org.cogchar.bind.mio.robot.svc.ModelBlendingRobotServiceContext
import org.friendularity.cpump.{ActorRefCPMsgTeller, CPStrongTeller}
import org.friendularity.dull.SpecialAppPumpSpace
import org.friendularity.respire._

/**
  * Created by Owner on 6/8/2016.
  */
trait NavUiAppSvc extends VarargsLogging {
	def postPatientCharCreateRq(dualBodyID : Ident, fullHumaCfg : HumanoidFigureConfig,
								mbrsc : ModelBlendingRobotServiceContext, answerTeller : CPStrongTeller[VWBodyNotice])

	def makeExoBodyUserTeller(parentARF : ActorRefFactory, ebuActorName : String, userLogic : ExoBodyUserLogic) : CPStrongTeller[VWBodyNotice] = {
		val ebuActor = ExoActorFactory.makeExoBodyUserActor(parentARF, ebuActorName, userLogic)
		val ebuTeller : CPStrongTeller[VWBodyNotice] = new ActorRefCPMsgTeller[VWBodyNotice](ebuActor)
		ebuTeller
	}
}

// "App" here means FriendU app, not a JME3 "app".  The latter is made during launchSimRenderSpace at bottom.
class NavUiAppImpl(myAkkaSys : ActorSystem) extends NavUiAppSvc {

	lazy private val myStandalonePumpSpace = new SpecialAppPumpSpace(myAkkaSys)

	lazy private val vwBossAR: ActorRef = VWorldActorFactoryFuncs.makeVWorldBoss(myAkkaSys, "vworldBoss_818")
	lazy private val vwBossTeller = new ActorRefCPMsgTeller(vwBossAR)

	lazy private val standPumpTestCtxName = NavUiTestPublicNames.cpumpName
	lazy private val standPumpCtxActorRef : ActorRef = myStandalonePumpSpace.findTopActorRef(standPumpTestCtxName)
	lazy private val standPumpAdminTeller = new ActorRefCPMsgTeller(standPumpCtxActorRef)


	/*
	// Direct approach
	lazy private val outerCtxName = "nav_ui_outer"
	lazy private val outerCtxActorRef : ActorRef = makeCustomOuterActor(myAkkaSys)
	lazy private val outerTellerDirect = new ActorRefCPMsgTeller(outerCtxActorRef)

	private def makeCustomOuterActor(akkaSys: ActorSystem) : ActorRef = {
		val vwbossActorProps = Props(classOf[OuterDirectActor])
		val vwbActorRef : ActorRef = akkaSys.actorOf(vwbossActorProps, outerCtxName)
		vwbActorRef
	}
	*/

	// Jobby approach
	lazy private val goodyTestSenderLogic = new PatientSender_GoodyTest {}
	lazy private val goodyTestSenderTrigTeller  = OuterJobbyLogic_MasterFactory.makeOoLogicAndTeller(goodyTestSenderLogic, myAkkaSys, "goodyTstSndr")

	lazy private val charAdmForwarderLogic = new PatientForwarder_CharAdminTest {}
	lazy private val charAdmSenderTrigTeller  = OuterJobbyLogic_MasterFactory.makeOoLogicAndTeller(charAdmForwarderLogic, myAkkaSys, "charAdmForwarder")

	// Custom-outer approach


	def sendSetupMsgs_Async {
		val hpatMsg = new VWARM_GreetFromPumpAdmin(standPumpAdminTeller)
		info2("Sending msg={} to VWBossTeller : {}", hpatMsg, vwBossTeller)
		vwBossTeller.tellCPMsg(hpatMsg)

		sendVWSetup_Conf()

		sendVWSetup_Lnch()

		registePostInitWaiters()
	}
	def sendVWSetup_Conf() : Unit = {
		val msg = new VWSetupRq_Conf
		vwBossTeller.tellCPMsg(msg)
	}

	def sendVWSetup_Lnch() : Unit = {
		val msg = new VWSetupRq_Lnch
		vwBossTeller.tellCPMsg(msg)
	}
	def registePostInitWaiters() : Unit = {
		// Each of these the results of happy startup, to trigger further ops.
		// The VWPTRendezvous logic makes sure that each such waiter gets notified regardless of message order,
		// so it is OK to send them after init is already complete (although usually it won't be).
		val goodyTstRegMsg = new VWARM_FindPublicTellers(goodyTestSenderTrigTeller)
		debug2("Sending goody-reg-msg={} to VWBossTeller : {}", goodyTstRegMsg, vwBossTeller)
		vwBossTeller.tellCPMsg(goodyTstRegMsg)

		val charAdmRegMsg = new VWARM_FindPublicTellers(charAdmSenderTrigTeller)
		vwBossTeller.tellCPMsg(charAdmRegMsg)

	}
	// Could easily send this in as an actor msg, probably will soon, but trying to prevent confusion across API layers.
	def appendCharAdmRq(chrAdmRq : VWCharAdminRq) : Unit = charAdmForwarderLogic.appendInboundRq(chrAdmRq)

	override def postPatientCharCreateRq(dualBodyID : Ident, fullHumaCfg : HumanoidFigureConfig,
										 mbrsc : ModelBlendingRobotServiceContext, answerTeller : CPStrongTeller[VWBodyNotice]) : Unit = {
		val ccrq = VWCreateCharRq(dualBodyID, fullHumaCfg, mbrsc, answerTeller)
		appendCharAdmRq(ccrq)
	}
	def testDetachedGS : Unit = {
		val dgst = new DetachedGST{}
		dgst.gridSpaceTest
	}
	// registerAvatarConfigRepoClient(bunCtx, erc);
}