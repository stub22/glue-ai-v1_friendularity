package org.friendularity.navui

import akka.actor.ActorSystem
import org.appdapter.core.name.Ident
import org.cogchar.api.humanoid.HumanoidFigureConfig
import org.cogchar.bind.mio.robot.svc.ModelBlendingRobotServiceContext
import org.friendularity.cpmsg.CPStrongTeller
import org.friendularity.qpc.OffersQpidSvcs
import org.friendularity.respire.DetachedGST
import org.friendularity.vwimpl.MakesVWBoss
import org.friendularity.vwmsg.{VWBodyMakeRq, VWBodyNotice, VWSetupRq_Lnch, VWSetupRq_Conf}

/**
  * Created by Owner on 8/8/2016.
  */
class NavUiAppImpl(myAkkaSys : ActorSystem) extends NavUiAppSvc with NavPumpSpaceOwner
			with AppServiceHandleGroup with MakesVWBoss with OffersQpidSvcs {

	override protected def getAkkaSys : ActorSystem = myAkkaSys
	override protected def findAppQpidSvcOffering_opt : Option[OffersQpidSvcs] = Some(this)

	// Desired effect of these messages is to launch a running OpenGL v-world, ready for characters and other content
	// to be inserted into it.  Those facilities are available via actors defined in PubTeller replies sent to the
	// postInitWaiters.
	def sendSetupMsgs_Async {
		val vbt = getVWBossTeller // triggers creation of Boss Actor + teller
		sendGreetMsgs_Async(vbt)  // Validates actor messaging, otherwise no significant effect as of 2016-06-16

		// As of 2016-06-16 an earlier experiment with "VWSetup_Conf" is disabled, and we proceed directly to launch.
		// When active, this conf step causes a duplicate copy of legacy config repo to be
		// loaded, which we don't have any actual use for presently.   Under
		// OSGi there is an outer copy of that same config repo, used for launching
		// the avatar bodies.
		// sendVWSetup_Conf()

		sendVWSetup_Lnch() // First and only call that really makes async launch happen, as of 2016-06-17

		registerPostInitWaiters(vbt) // Setup listeners to do more stuff at appropriate times, as VWorld init completes.

	}

	def sendVWSetup_Conf_IsUnused() : Unit = {
		val msg = new VWSetupRq_Conf
		getVWBossTeller.tellCPMsg(msg)
	}

	def sendVWSetup_Lnch() : Unit = {
		val msg = new VWSetupRq_Lnch
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