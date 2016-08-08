package org.friendularity.navui

import akka.actor.{ActorRef, Props, ActorSystem}
import org.appdapter.fancy.log.VarargsLogging
import org.friendularity.akact.KnowsAkkaSys
import org.friendularity.cpmsg.{ActorRefCPMsgTeller, CPStrongTeller}
import org.friendularity.field.MsgToStatusSrc
import org.friendularity.qpc.OffersQpidSvcs
import org.friendularity.vwmsg.{VWBodyLifeRq, VWARM_FindPublicTellers, VWorldRequest, VWorldPublicTellers}

/**
  * Created by Stub22 on 8/8/2016.
  */
trait AppServiceHandleGroup extends KnowsAkkaSys with VarargsLogging {
	lazy val akkaSys = getAkkaSys
	// Jobby approach to actor launch is used here for our outer actors, experimentally.
	lazy private val goodyTestSenderLogic = new PatientSender_GoodyTest {}
	lazy private val goodyTestSenderTrigTeller  = OuterJobbyLogic_MasterFactory.makeOoLogicAndTeller(goodyTestSenderLogic, akkaSys, "goodyTstSndr")

	lazy private val charAdmForwarderLogic = new PatientForwarder_CharAdminTest {}
	lazy private val charAdmSenderTrigTeller  = OuterJobbyLogic_MasterFactory.makeOoLogicAndTeller(charAdmForwarderLogic, akkaSys, "charAdmForwarder")

	lazy private val bonusStagingLogic = new PatientSender_BonusStaging {}
	lazy private val bonusStagingTrigTeller  = OuterJobbyLogic_MasterFactory.makeOoLogicAndTeller(bonusStagingLogic, akkaSys, "bonusStagingRequester")

	protected def findQpidSvcOffering_opt : Option[OffersQpidSvcs] = None

	lazy private val statusTickPumpLogic = new OuterAppPumpSetupLogic {
		override protected def getAkkaSystem : ActorSystem = getAkkaSys
		override protected def makePubStatTempBypassTeller_opt(vwpt: VWorldPublicTellers) : Option[CPStrongTeller[MsgToStatusSrc]] = {

			val statPubLogic = new VWStatPubLogic {
				override protected def getPubTellers : VWorldPublicTellers = vwpt
				override protected def getQpidSvcOffering_opt = findQpidSvcOffering_opt
			}
			val statPubActorProps = Props(classOf[VWStatPubActor], statPubLogic)
			val statPubActorRef : ActorRef = getAkkaSystem.actorOf(statPubActorProps, "statPubActr")
			val statPubTeller = new ActorRefCPMsgTeller[MsgToStatusSrc](statPubActorRef)
			Option(statPubTeller)
		}
	}
	lazy private val statusTickTrigTeller  = OuterJobbyLogic_MasterFactory.makeOoLogicAndTeller(statusTickPumpLogic, akkaSys, "statusTickPumpSetup")
	def registerPostInitWaiters(vbt : CPStrongTeller[VWorldRequest]) : Unit = {

		// Each of these the results of happy startup, to trigger further ops.
		// The VWPTRendezvous logic makes sure that each such waiter gets notified regardless of message order,
		// so it is OK to send them after init is already complete (although usually it won't be).
		val goodyTstRegMsg = new VWARM_FindPublicTellers(goodyTestSenderTrigTeller)
		debug2("Sending goody-listener--reg={} to VWBossTeller : {}", goodyTstRegMsg, vbt)
		vbt.tellCPMsg(goodyTstRegMsg)

		val charAdmRegMsg = new VWARM_FindPublicTellers(charAdmSenderTrigTeller)
		debug2("Sending char-admin-listener-reg={} to VWBossTeller : {}", charAdmRegMsg, vbt)
		vbt.tellCPMsg(charAdmRegMsg)

		val bonusStageRegMsg = new VWARM_FindPublicTellers(bonusStagingTrigTeller)
		debug2("Sending bonusStage-listener-reg={} to VWBossTeller : {}", bonusStageRegMsg, vbt)
		vbt.tellCPMsg(bonusStageRegMsg)

		val tickPumpRegMsg = new VWARM_FindPublicTellers(statusTickTrigTeller)
		debug2("Sending tickPump-listener-reg={} to VWBossTeller : {}", tickPumpRegMsg, vbt)
		vbt.tellCPMsg(tickPumpRegMsg)
	}
	// Can't so directly send this yet as  actor msg.  There is a kind of implicit rendezvous going on here
	// between  the optional MechIO connection, the legacy-repo-based humaConfig, and the launched VWorld actors.
	def appendCharAdmRq(chrAdmRq : VWBodyLifeRq) : Unit = charAdmForwarderLogic.appendInboundRq(chrAdmRq)

}