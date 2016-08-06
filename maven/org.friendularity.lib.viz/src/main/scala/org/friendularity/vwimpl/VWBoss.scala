package org.friendularity.vwimpl

import akka.actor.{ActorLogging, Actor, ActorContext, ActorRef}
import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.bind.midi.in.TempMidiBridge
import org.cogchar.render.goody.basic.{BasicGoodyCtxImpl, BasicGoodyCtx}
import org.cogchar.render.sys.registry.RenderRegistryClient
import org.cogchar.render.sys.window.WindowStatusMonitor
import org.friendularity.cpmsg.ActorRefCPMsgTeller
import org.friendularity.vwmsg.{VWOverlayRq, VWStageRqMsg, VWShapeCreateRq, VWorldInternalNotice, VWBodyLifeRq, VWPubTellersMsgImpl, VWGoodyRqActionSpec, LesserIngred, BodyMgrIngred, VWorldNotice, VWSetupResultsNotice, VWARM_GreetFromPumpAdmin, VWARM_FindPublicTellers, VWAdminRqMsg, VWSetupRq_Lnch, VWSetupRq_Conf, VWorldRequest}

/**
  * Created by Owner on 6/15/2016.
  */
trait VWorldBossLogic [VWSM <: VWorldSysMgr] extends VarargsLogging with VWPTRendezvous {
	protected def getSysMgr : VWSM

	protected def processVWorldRequest(vwmsg : VWorldRequest, slfActr : ActorRef, localActorCtx : ActorContext): Unit = {
		val sysMgr = getSysMgr
		vwmsg match {
			// Heart of the V-World setup is done here:
			case vwSetupLnchMsg : VWSetupRq_Lnch => launchSimRenderSpace(vwSetupLnchMsg, slfActr, localActorCtx)

			case otherAdminRq : VWAdminRqMsg => processVWAdminMsg(otherAdminRq, localActorCtx)

			// Currently this setup-conf is unnecessary, so client is actually sending it.
			case vwSetupCnfMsg : VWSetupRq_Conf =>
				error1("The VWCnfMgr code is disabled, but we received: {}", vwSetupCnfMsg)
				// unused_FakeloadConf(vwSetupCnfMsg, localActorCtx)

		}
	}

	protected def processVWAdminMsg(vwmsg : VWAdminRqMsg, localActorCtx : ActorContext): Unit = {
		val sysMgr = getSysMgr
		info3("VWBoss processing  msg={} with sysMgr={} and actCtx={}", vwmsg, sysMgr, localActorCtx)
		vwmsg match {
			case gfpa : VWARM_GreetFromPumpAdmin => {
				info1("VWBoss says thanks for the greet msg: {}", gfpa)
			}

			case fpt : VWARM_FindPublicTellers => {
				info1("VWBoss registering a pub-teller listener: {}", fpt)
				addVWPTListener(fpt.answerTeller)
			}
		}
	}
	// Handle event from inside this boss, translate into messages for boss-clients.
	protected def processVWorldInternalNotice(vwmsg : VWorldNotice, slfActr : ActorRef, localActorCtx : ActorContext): Unit = {
		vwmsg match {
			case setupResults: VWSetupResultsNotice => {
				val lesserIng = setupResults.lesserIngred
				val bodyMgrIng = setupResults.bodyMgrIngred
				val updAtchr = setupResults.updAtchr
				val tmb_opt = setupResults.tmb_opt
				completeBossSetupAndPublish(lesserIng, bodyMgrIng, updAtchr, tmb_opt, localActorCtx)
			}
		}
	}

	protected def launchSimRenderSpace(vwLnchMsg : VWSetupRq_Lnch, slfActr : ActorRef,  localActorCtx : ActorContext): Unit = {
		// TODO:  We want this launch process to call us back with the ingredients chef needs to proceed.
		// The soonest that *could* happen is *during* JME3.start(), but we would actually prefer it be later,
		// during isolatedInitTask.
		val bsim = new SimBalloonAppLauncher {}
		val resultsTeller = new ActorRefCPMsgTeller(slfActr) // vwLnchMsg.resultsTeller
		info0("makeSimSpace Calling bsim.setup")
		bsim.setup(resultsTeller)
		info0("makeSimSpace END - vworld is now running, but delayed setup jobs may still be running/pending")
	}

	// Crucial method which wraps the internal setup results handles with a set of public actors,
	// and then publishes that API for outer clients to use.
	def completeBossSetupAndPublish(lesserIngred: LesserIngred, bmi : BodyMgrIngred,
									updAtchr : UpdateAttacher, tmb_opt : Option[TempMidiBridge],
									localActorCtx : ActorContext): Unit = {
		//
		info1("Got setup result (lesser) ingredients: {}", lesserIngred)
		val rrc : RenderRegistryClient = lesserIngred.getRendRegClient
		val winStatMon : WindowStatusMonitor = lesserIngred.getWindowStatusMonitor
		val bgc : BasicGoodyCtx = new BasicGoodyCtxImpl(rrc, winStatMon)
		val sysMgr = getSysMgr

		val goodyActorRef = VWorldActorFactoryFuncs.makeVWGoodyActor(localActorCtx, "googoo", bgc)
		val goodyTeller = new ActorRefCPMsgTeller[VWGoodyRqActionSpec](goodyActorRef)

		val pmrc = bmi.getPMRC
		val cmgrCtx = new VWCharMgrCtxImpl(pmrc)
		val charAdmActorRef = VWorldActorFactoryFuncs.makeVWCharAdminActor(localActorCtx, "charAdm", cmgrCtx)
		val charAdmTeller  = new ActorRefCPMsgTeller[VWBodyLifeRq](charAdmActorRef)

		val shaperActorRef = VWorldActorFactoryFuncs.makeVWShaperActor(localActorCtx, "shaper", rrc)
		val shaperTeller  = new ActorRefCPMsgTeller[VWShapeCreateRq](shaperActorRef)

		val stagerCtx = new StageCtxImpl(pmrc, updAtchr, tmb_opt)
		val stageActorRef = VWorldActorFactoryFuncs.makeVWStageActor(localActorCtx, "stager", stagerCtx)
		val stageTeller  = new ActorRefCPMsgTeller[VWStageRqMsg](stageActorRef)

		val ovlActorRef = VWorldActorFactoryFuncs.makeVWOverlayActor(localActorCtx, "overlay", rrc)
		val ovlTeller = new ActorRefCPMsgTeller[VWOverlayRq](ovlActorRef)

		// Now the boss can publish this nice public API offering message, providing network-ready
		// handles to all the services defined above.

		val vwpt = new VWPubTellersMsgImpl(goodyTeller, charAdmTeller, shaperTeller, stageTeller, ovlTeller)
		setVWPT(vwpt)
	}

}
// Top level actor, directly handles only the grossest VWorld system start/restart/shutdown
// kinds of messages.  More importantly, boss serves as top actor supervisor for all internal
// vworld actors.   Conceptually it also marks the boundary between inside and the outside of the vworld.
// The sysMgr and hackStrap are the *only* places that any complex state higgledy-piggledy
// may take place, and it is our goal to minimize that state and code.  (That's why we give
// these things names.  Shared state is bad, so we won't let it be vague or implicit!).

// Child actors of this boss should not accept unserializable constructor args (or messages).

class VWorldBossActor[VWSM <: VWorldSysMgr](sysMgr : VWSM, hackStrap : VWorldStrap)
			extends Actor with ActorLogging with VWorldBossLogic[VWSM]  {
	def receive = {
		// Construction of any other actors used with the ctx must happen within this handler.
		// Those actors may be sent back in receiptMsgs to answerTellers embedded in the input msg.
		// Note that "context" here is a pseudo-field of the actor.
		case vwrq : VWorldRequest => {
			processVWorldRequest(vwrq, self, context)
		}
		case vwnot : VWorldInternalNotice => {
			processVWorldInternalNotice(vwnot, self, context)
		}
	}

	override protected def getSysMgr : VWSM = sysMgr
}