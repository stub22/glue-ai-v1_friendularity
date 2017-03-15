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

package org.friendularity.vw.impl.boss

import java.awt.Image
import javax.swing.JFrame

import akka.actor.{Actor, ActorContext, ActorLogging, ActorRef}
import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.bind.midi.in.TempMidiBridge
import org.cogchar.render.goody.basic.BasicGoodyCtx
import org.cogchar.render.sys.registry.RenderRegistryClient
import org.cogchar.render.sys.window.WindowStatusMonitor
import org.friendularity.infra.akact.KnowsAkkaSys
import org.friendularity.infra.cpmsg.{ActorRefCPMsgTeller, CPStrongTeller}
import org.friendularity.navui.NavAppCloser
import org.friendularity.vw.impl.bdy.VWCharMgrCtxImpl
import org.friendularity.vw.impl.stg.StageCtxImpl
import org.friendularity.vw.impl.sys.{VWorldStrap, UpdateAttacher, SimBalloonAppLauncher, VWorldActorFactoryFuncs, VWPTRendezvous, VWorldSysMgr, VWJdkAwtCanvasMgr}
import org.friendularity.vw.mprt.ingred.{BodyMgrIngred, LesserIngred}
import org.friendularity.vw.msg.adm.{VWARM_FindPublicTellers, VWARM_GreetFromPumpAdmin, VWAdminRqMsg, VWSetSwingCanvasBranding, VWSetupRq_Conf}
import org.friendularity.vw.msg.bdy.VWBodyLifeRq
import org.friendularity.vw.msg.cor.{VWOverlayRq, VWContentRq, VWorldNotice, VWorldRequest}
import org.friendularity.vw.msg.intrn.{VWSetupRq_Lnch, VWSetupResultsNotice, VWorldInternalNotice}

import org.friendularity.vw.msg.pub.VWPubTellersMsgImpl

import org.friendularity.vw.msg.stg.VWStageRqMsg
import org.friendularity.vw.msg.ta.VWRqTAWrapper

/**
  * Created by Stub22 on 6/15/2016.
  */
trait VWorldBossLogic [VWSM <: VWorldSysMgr] extends VarargsLogging with VWPTRendezvous {

	lazy val mySimCnvsLnchMgr = new VWSimCanvasLnchMgr {}

	protected def getSysMgr : VWSM

	protected def processVWorldRequest(vwmsg : VWorldRequest, slfActr : ActorRef, localActorCtx : ActorContext): Unit = {
		val sysMgr = getSysMgr
		vwmsg match {
			// Heart of the V-World setup is done here:
			case vwSetupLnchMsg : VWSetupRq_Lnch => mySimCnvsLnchMgr.launchSimRenderSpace(vwSetupLnchMsg, slfActr, localActorCtx)

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
            
            case sscb: VWSetSwingCanvasBranding =>{
				mySimCnvsLnchMgr.handleCanvasBranding(sscb)
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

	// Crucial method which wraps the internal setup results handles with a set of public actors,
	// and then publishes that API for outer clients to use.
	def completeBossSetupAndPublish(lesserIngred: LesserIngred, bmi : BodyMgrIngred,
									updAtchr : UpdateAttacher, tmb_opt : Option[TempMidiBridge],
									localActorCtx : ActorContext): Unit = {
		//
		info1("Got setup result (lesser) ingredients: {}", lesserIngred)
		val rrc : RenderRegistryClient = lesserIngred.getRendRegClient
		val winStatMon : WindowStatusMonitor = lesserIngred.getWindowStatusMonitor

		val sysMgr = getSysMgr

		val pmrc = bmi.getPMRC
		val cmgrCtx = new VWCharMgrCtxImpl(pmrc)
		val charAdmActorRef = VWorldActorFactoryFuncs.makeVWCharAdminActor(localActorCtx, "charAdm", cmgrCtx)
		val charAdmTeller  = new ActorRefCPMsgTeller[VWBodyLifeRq](charAdmActorRef)

		val shaperActorRef = VWorldActorFactoryFuncs.makeVWShaperActor(localActorCtx, "shaper", rrc)
		val shaperTeller  = new ActorRefCPMsgTeller[VWContentRq](shaperActorRef)

		val goodyActorRef = VWorldActorFactoryFuncs.makeVWGoodyActor(localActorCtx, "googoo", shaperTeller)
		val goodyTeller = new ActorRefCPMsgTeller[VWRqTAWrapper](goodyActorRef)


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
