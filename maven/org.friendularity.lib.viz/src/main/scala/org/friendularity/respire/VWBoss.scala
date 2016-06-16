package org.friendularity.respire

import akka.actor.{ActorLogging, Actor, ActorContext, ActorRef}
import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.render.goody.basic.{BasicGoodyCtxImpl, BasicGoodyCtx}
import org.cogchar.render.sys.registry.RenderRegistryClient
import org.cogchar.render.sys.window.WindowStatusMonitor
import org.friendularity.cpump.ActorRefCPMsgTeller
import org.friendularity.vwmsg.{VWorldInternalNotice, VWBodyLifeRq, VWPubTellersMsgImpl, VWGoodyRqActionSpec, LesserIngred, BodyMgrIngred, VWorldNotice, VWSetupResultsNotice, VWARM_GreetFromPumpAdmin, VWARM_FindPublicTellers, VWAdminRqMsg, VWSetupRq_Lnch, VWSetupRq_Conf, VWorldRequest}

/**
  * Created by Owner on 6/15/2016.
  */
trait VWorldBossLogic [VWSM <: VWorldSysMgr] extends VarargsLogging with VWPTRendezvous {
	protected def getSysMgr : VWSM

	protected def processVWorldRequest(vwmsg : VWorldRequest, slfActr : ActorRef, localActorCtx : ActorContext): Unit = {
		val sysMgr = getSysMgr
		vwmsg match {
			// Currently this setup-conf is unnecessary, so client is actually sending it.
			case vwSetupCnfMsg : VWSetupRq_Conf => loadConf(vwSetupCnfMsg, localActorCtx)

			// Heart of the V-World setup is done here:
			case vwSetupLnchMsg : VWSetupRq_Lnch => launchSimRenderSpace(vwSetupLnchMsg, slfActr, localActorCtx)

			case otherAdminRq : VWAdminRqMsg => processVWAdminMsg(otherAdminRq, localActorCtx)

		}
	}

	protected def processVWAdminMsg(vwmsg : VWAdminRqMsg, localActorCtx : ActorContext): Unit = {
		val sysMgr = getSysMgr
		info3("Processing  msg={} with sysMgr={} and actCtx={}", vwmsg, sysMgr, localActorCtx)
		vwmsg match {
			case gfpa : VWARM_GreetFromPumpAdmin => {

			}
			/*
			case fgt : VWARM_FindGoodyTeller => {
			}
			*/
			case fpt : VWARM_FindPublicTellers => {
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
				notifySetupResults(lesserIng, bodyMgrIng, localActorCtx)
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

	// Currently unused
	protected def loadConf(vwConfMsg : VWSetupRq_Conf, localActorCtx : ActorContext): Unit = {
		// This is loading a complete copy of the legacy avatar config, but ...
		// it is not used in the current load of the vw-bodies/avatars, which is being
		// done under OSGi under direct control of the CCMIO_DemoActivator.
		val cnfMgr = new VWCnfMgr {}
		val profileGraph = cnfMgr.getProfileGraph
		info1("Got profileGraph: {}", profileGraph)
		val legConf_opt = cnfMgr.getLegConfERC_opt
	}

	def notifySetupResults(lesserIngred: LesserIngred, bmi : BodyMgrIngred, localActorCtx : ActorContext): Unit = {
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

		val vwpt = new VWPubTellersMsgImpl(goodyTeller, charAdmTeller)
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