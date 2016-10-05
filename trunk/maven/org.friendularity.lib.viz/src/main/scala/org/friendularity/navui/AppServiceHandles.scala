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

import akka.actor.{ActorRef, Props, ActorSystem}
import org.appdapter.fancy.log.VarargsLogging
import org.friendularity.akact.{FrienduActor, KnowsAkkaSys}
import org.friendularity.cpmsg.{ActorRefCPMsgTeller, CPStrongTeller}
import org.friendularity.field.MsgToStatusSrc
import org.friendularity.qpc.OffersVWorldServer
import org.friendularity.vwimpl.MakesVWTAReqRouterTeller
import org.friendularity.vwmsg.{VWRqTAWrapper, VWorldPublicTellers, VWBodyLifeRq, VWARM_FindPublicTellers, VWorldRequest}

/**
  * Created by Stub22 on 8/8/2016.
  */
trait ExtraSetupLogic {
	def doExtraSetup(vwpt : VWorldPublicTellers) : Unit
}
trait VWTARqRouterSetupLogic extends ExtraSetupLogic with MakesVWTAReqRouterTeller {
	protected def findRouterQpidSvcOffering_opt : Option[OffersVWorldServer] = None  // Override to plugin QPid services

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
			val binTgtTeller = makeBinSerRoutingTeller(vwpt)
			info1("Setting unified routing teller to binTgtTeller={}", binTgtTeller)
			//	srvRcvFeat.setSerBinListenTeller(binSerRouteTeller)
			qso.setUnifiedListenTeller(binTgtTeller)
		} else {
			warn1("Cannot setup routing, because no qpid services are offered to logic={}", this)
		}
	}
}
// Extend this trait in an app specific method.
// All the wiring of these lazy vals begins whenever someone calls registerPostInitWaiters,
// which can happen as soon as a "boss" teller is known.
trait AppServiceHandleGroup extends KnowsAkkaSys with VarargsLogging {
	private lazy val myAppAkkaSys = getAkkaSys

	// When this method is overridden by the app class, it is seen in nested our subclasses of TARqRouterSetupLogic and VWStatPubLogic.
	protected def findAppQpidSvcOffering_opt : Option[OffersVWorldServer] = None

	// Here instead of a dedicated rendezvous, we make the TARouter setup subord to
	// PatientSender_GoodyTest, but perhaps those roles should be reversed!
	lazy private val taRouterSetupLogic = new VWTARqRouterSetupLogic {
		override protected def findRouterQpidSvcOffering_opt : Option[OffersVWorldServer] = findAppQpidSvcOffering_opt

		override protected def getAkkaSys: ActorSystem = myAppAkkaSys
	}

	// Jobby approach to actor launch is used here for our outer actors, experimentally.
	// Each jobby can subordinately choose to define extraSetupTasks.
    // (ben)[2016-10-04]: Is just used as router for goody thing actions
	lazy private val goodyTestSenderLogic = new PatientSender_GoodyRouter {
    // Has fun shape object test
//	lazy private val goodyTestSenderLogic = new PatientSender_GoodyTest { 
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

	// Ask the supplied teller (presumed to be a "boss" teller) to send messages back to all our
	// interested parties, after the core "public" services are accessible.  That time could
	// be right away, or later.   OK to call this anytime to connect to an existing running vworld.
	def registerPostInitWaiters(bossTeller : CPStrongTeller[VWorldRequest]) : Unit = {

		// Each of these the results of happy startup, to trigger further ops.
		// The VWPTRendezvous logic makes sure that each such waiter gets notified regardless of message order,
		// so it is OK to send these after init is already complete (although usually it won't be
        
		val goodyTstRegMsg = new VWARM_FindPublicTellers(goodyTestSenderTrigTeller)
		debug2("Sending goody-listener--reg={} to VWBossTeller : {}", goodyTstRegMsg, bossTeller)
		bossTeller.tellCPMsg(goodyTstRegMsg)

		val charAdmRegMsg = new VWARM_FindPublicTellers(charAdmSenderTrigTeller)
		debug2("Sending char-admin-listener-reg={} to VWBossTeller : {}", charAdmRegMsg, bossTeller)
		bossTeller.tellCPMsg(charAdmRegMsg)

		val bonusStageRegMsg = new VWARM_FindPublicTellers(bonusStagingTrigTeller)
		debug2("Sending bonusStage-listener-reg={} to VWBossTeller : {}", bonusStageRegMsg, bossTeller)
		bossTeller.tellCPMsg(bonusStageRegMsg)

		val tickPumpRegMsg = new VWARM_FindPublicTellers(statusTickTrigTeller)
		debug2("Sending tickPump-listener-reg={} to VWBossTeller : {}", tickPumpRegMsg, bossTeller)
		bossTeller.tellCPMsg(tickPumpRegMsg)
	}
	// Can't so directly send this yet as  actor msg.  There is a kind of implicit rendezvous going on here
	// between  the optional MechIO connection, the legacy-repo-based humaConfig, and the launched VWorld actors.
	def appendCharAdmRq(chrAdmRq : VWBodyLifeRq) : Unit = charAdmForwarderLogic.appendInboundRq(chrAdmRq)

}