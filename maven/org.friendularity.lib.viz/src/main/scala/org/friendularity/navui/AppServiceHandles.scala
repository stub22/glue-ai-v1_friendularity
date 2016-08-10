package org.friendularity.navui

import akka.actor.{ActorRef, Props, ActorSystem}
import org.appdapter.fancy.log.VarargsLogging
import org.friendularity.akact.{FrienduActor, KnowsAkkaSys}
import org.friendularity.cpmsg.{ActorRefCPMsgTeller, CPStrongTeller}
import org.friendularity.field.MsgToStatusSrc
import org.friendularity.qpc.OffersQpidSvcs
import org.friendularity.vwmsg.{VWGoodyRqActionSpec, VWorldPublicTellers, VWBodyLifeRq, VWARM_FindPublicTellers, VWorldRequest}

/**
  * Created by Stub22 on 8/8/2016.
  */
trait ThingActReqRouterLogic extends VarargsLogging {
	def routeRq(vwgrq: VWGoodyRqActionSpec) : Unit = {
		val tellers = getVWPubTellers
		info1("Router handling TARq in binary format: {}", vwgrq)
	}
	def getVWPubTellers : VWorldPublicTellers
}
class ThingActReqRouterActor(routerLogic : ThingActReqRouterLogic) extends FrienduActor {
	override def receive = {
		case vwgrq: VWGoodyRqActionSpec => {
			routerLogic.routeRq(vwgrq)
		}
		case other => {
			getLogger().warn("ThingActReqRouterActor received unexpected message: {}", other)
		}
	}
}
trait ExtraSetupLogic {
	def doExtraSetup(vwpt : VWorldPublicTellers) : Unit
}
trait TARqRouterSetupLogic extends ExtraSetupLogic with KnowsAkkaSys with VarargsLogging {
	protected def findRouterQpidSvcOffering_opt : Option[OffersQpidSvcs] = None  // Override to plugin QPid services

	override def doExtraSetup(vwpt : VWorldPublicTellers) : Unit = {
		info0("TARqRouterSetupLogic is calling setupRouting")
		setupRouting(vwpt)
		info0("TARqRouterSetupLogic finished setupRouting")
	}
	private def setupRouting(vwpt : VWorldPublicTellers) : Unit = {
		val qpidSvcOffer_opt = findRouterQpidSvcOffering_opt
		info1("qpidSvcOffer_opt = {}", qpidSvcOffer_opt)
		if (qpidSvcOffer_opt.isDefined) {
			val qso = qpidSvcOffer_opt.get
			val srvRcvFeat = qso.getServerReceiveFeature
			info1("Got srvFeatAcc={}", srvRcvFeat)
			val binSerRouteTeller = makeBinSerRoutingTeller(vwpt)
			info1("Setting bin-ser routing teller to {}", binSerRouteTeller)
			srvRcvFeat.setSerBinListenTeller(binSerRouteTeller)
		} else {
			warn1("Cannot setup routing, because no qpid services are offered to logic={}", this)
		}
	}
	private def makeBinSerRoutingTeller(vwpt : VWorldPublicTellers) : CPStrongTeller[VWGoodyRqActionSpec] = {
		val akkaSys = getAkkaSys
		val routerLogic = new ThingActReqRouterLogic {
			val myVWPT : VWorldPublicTellers = vwpt
			override def getVWPubTellers : VWorldPublicTellers = myVWPT
		}
		info1("Made routerLogic={}", routerLogic)
		val routerActorProps = Props(classOf[ThingActReqRouterActor], routerLogic)
		val routerActorRef : ActorRef = akkaSys.actorOf(routerActorProps, "taBinSerRouterActr")
		info1("Made routerActorRef={}", routerActorRef)
		val routerTeller = new ActorRefCPMsgTeller[VWGoodyRqActionSpec](routerActorRef)
		routerTeller
	}
}
// Extend this trait in an app specific method
trait AppServiceHandleGroup extends KnowsAkkaSys with VarargsLogging {
	private lazy val myAppAkkaSys = getAkkaSys

	// When this method is overridden by the app class, it is seen in nested our subclasses of TARqRouterSetupLogic and VWStatPubLogic.
	protected def findAppQpidSvcOffering_opt : Option[OffersQpidSvcs] = None

	lazy private val taRouterSetupLogic = new TARqRouterSetupLogic {
		override protected def findRouterQpidSvcOffering_opt : Option[OffersQpidSvcs] = findAppQpidSvcOffering_opt

		override protected def getAkkaSys: ActorSystem = myAppAkkaSys
	}

	// Jobby approach to actor launch is used here for our outer actors, experimentally.
	// Each jobby can subordinately choose to define extraSetupTasks.
	lazy private val goodyTestSenderLogic = new PatientSender_GoodyTest {
		// Notice how more extra setup tasks could be used in place of some of the OuterJobbyLogics below.
		override protected def getExtraSetupTasks() : List[ExtraSetupLogic] = List(taRouterSetupLogic)
	}
	lazy private val goodyTestSenderTrigTeller  = OuterJobbyLogic_MasterFactory.makeOoLogicAndTeller(goodyTestSenderLogic, myAppAkkaSys, "goodyTstSndr")

	lazy private val charAdmForwarderLogic = new PatientForwarder_CharAdminTest {}
	lazy private val charAdmSenderTrigTeller  = OuterJobbyLogic_MasterFactory.makeOoLogicAndTeller(charAdmForwarderLogic, myAppAkkaSys, "charAdmForwarder")

	lazy private val bonusStagingLogic = new PatientSender_BonusStaging {}
	lazy private val bonusStagingTrigTeller  = OuterJobbyLogic_MasterFactory.makeOoLogicAndTeller(bonusStagingLogic, myAppAkkaSys, "bonusStagingRequester")


	lazy private val statusTickPumpLogic = new OuterAppPumpSetupLogic {
		override protected def getAkkaSystem : ActorSystem = getAkkaSys
		override protected def makePubStatTempBypassTeller_opt(vwpt: VWorldPublicTellers) : Option[CPStrongTeller[MsgToStatusSrc]] = {

			val statPubLogic = new VWStatPubLogic {
				override protected def getPubTellers : VWorldPublicTellers = vwpt
				override protected def getQpidSvcOffering_opt = findAppQpidSvcOffering_opt
			}
			val statPubActorProps = Props(classOf[VWStatPubActor], statPubLogic)
			val statPubActorRef : ActorRef = getAkkaSystem.actorOf(statPubActorProps, "statPubActr")
			val statPubTeller = new ActorRefCPMsgTeller[MsgToStatusSrc](statPubActorRef)
			Option(statPubTeller)
		}
	}
	lazy private val statusTickTrigTeller  = OuterJobbyLogic_MasterFactory.makeOoLogicAndTeller(statusTickPumpLogic, myAppAkkaSys, "statusTickPumpSetup")

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