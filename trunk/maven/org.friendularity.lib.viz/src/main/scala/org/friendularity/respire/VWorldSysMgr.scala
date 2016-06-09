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

package org.friendularity.respire

import akka.actor._

import org.appdapter.fancy.log.VarargsLogging
import org.appdapter.fancy.rclient.EnhancedLocalRepoClient
import org.cogchar.blob.entry.EntryHost
import org.cogchar.render.app.humanoid.HumanoidRenderContext
import org.cogchar.render.goody.basic.{BasicGoodyCtxImpl, BasicGoodyCtx}
import org.cogchar.render.sys.goody.{GoodyModularRenderContext, GoodyRenderRegistryClient}
import org.cogchar.render.sys.registry.RenderRegistryClient
import org.cogchar.render.sys.window.WindowStatusMonitor
import org.friendularity.appro.TestRaizLoad
import org.friendularity.cpump.{ActorRefCPMsgTeller, CPStrongTeller}
import com.hp.hpl.jena.rdf.model.{Model => JenaModel}
import org.friendularity.rbody.DualBodyRecord

/**
  * Created by Owner on 4/14/2016.
  */

// Legit state of a running VWorld system is managed by an instance of VWorldSysMgr.
trait VWorldSysMgr {
	// See VWPTRendezvous
	// def findPublicTellers : VWorldPublicTellers

//	def findGoodyCtx_opt : Option[BasicGoodyCtx] = None
}
// (hack)Strap holds any icky extra shared state that we temporarily want to pass to VWorldBossActor.
trait VWorldStrap {
//	def	getPumpCtx : DullPumpCtx
}

trait VWCnfMgr extends VarargsLogging {
	lazy private val myMergedProfileJM : JenaModel = makeMergedProfileGraph("Temporarily unused selector args")
	private val loadLegacyConf = true
	lazy private val legConfERC_opt: Option[EnhancedLocalRepoClient] = {
		if (loadLegacyConf) Option(buildLegacyConfERC(myMergedProfileJM)) else None
	}
	def getProfileGraph = myMergedProfileJM
	def getLegConfERC_opt = legConfERC_opt
	protected def makeMergedProfileGraph(profileSelectorArgs : String) : JenaModel = {
		val tchunkEHost: EntryHost = TestRaizLoad.getUnitTestResourceEntryHost
		// TODO - Process the profileSelectorArgs
		val mergedProfileGraph: JenaModel = TestRaizLoad.getMergedProfileGraph_RegularDesktop(tchunkEHost)
		mergedProfileGraph
	}
	protected def buildLegacyConfERC(mergedProfGraph : JenaModel) : EnhancedLocalRepoClient = {
		// Legacy config load section, gradually becoming obsolete:
		// Under OSGi (e.g. CCMIO), old PumaBoot process is set up by attachVizTChunkLegConfRepo(BundleContext bunCtx).
		//val tchunkEHost: EntryHost = TestRaizLoad.makeBundleEntryHost(TestRaizLoad.getClass)

		val tchunkEHost: EntryHost = TestRaizLoad.getUnitTestResourceEntryHost

		// TODO: Find this URI from either a query or an onto-constant
		val vzBrkRcpUriTxt: String = TestRaizLoad.vzpLegCnfBrkrRcpUriTxt

		val legConfERC = TestRaizLoad.makeAvatarLegacyConfigRepo(mergedProfGraph, vzBrkRcpUriTxt, tchunkEHost)
		getLogger.info("legConfERC={}", legConfERC)
		legConfERC
	}

}
trait VWPTRendezvous {
	private var myListeners : List[CPStrongTeller[VWorldPublicTellers]] = Nil
	private var myVWPT_opt : Option[VWorldPublicTellers] = None

	def addVWPTListener (teller: CPStrongTeller[VWorldPublicTellers]) : Unit = {
		synchronized{
			myListeners = teller :: myListeners
			if (myVWPT_opt.isDefined) {
				teller.tellStrongCPMsg(myVWPT_opt.get)
			}
		}
	}
	protected def notifyVWPTListeners(vwpt : VWorldPublicTellers) : Unit = {
		synchronized {
			myListeners.foreach (_.tellStrongCPMsg (vwpt) )
		}
	}
	def setVWPT(vwpt : VWorldPublicTellers): Unit = {
		synchronized {
			myVWPT_opt = Option (vwpt)
			notifyVWPTListeners (vwpt)
		}
	}
}
trait VWorldBossLogic [VWSM <: VWorldSysMgr] extends VarargsLogging with VWPTRendezvous {
	protected def getSysMgr : VWSM

	protected def processVWorldRequest(vwmsg : VWorldRequest, slfActr : ActorRef, localActorCtx : ActorContext): Unit = {
		val sysMgr = getSysMgr
		vwmsg match {

			case vwSetupCnfMsg : VWSetupRq_Conf => loadConf(vwSetupCnfMsg, localActorCtx)

			case vwSetupLnchMsg : VWSetupRq_Lnch => launchSimRenderSpace(vwSetupLnchMsg, slfActr, localActorCtx)

			case adminMsg : VWAdminRqMsg => processVWAdminMsg(adminMsg, localActorCtx)

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

	protected def loadConf(vwConfMsg : VWSetupRq_Conf, localActorCtx : ActorContext): Unit = {
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
		val charAdmTeller  = new ActorRefCPMsgTeller[VWCharAdminRq](charAdmActorRef)

		val vwpt = new VWPubTellersImpl(goodyTeller, charAdmTeller)
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
class VWSysMgrImpl extends VWorldSysMgr {

	// override def findPublicTellers : VWorldPublicTellers = new VWorldPublicTellers{}

}


class VWStrapImpl extends VWorldStrap {
	// Good news - the hackStrap is still empty!  Let's hope it stays that way.
}
object VWorldActorFactoryFuncs {

	// These are for "core" actors used within the PublicTellers boundary, not for the Outer+Exo actors.

	// Following akka pattern, parentARF is either an ActorSystem (root) OR ActorContext (not-root)
	def makeVWorldBoss(parentARF : ActorRefFactory, bossActorName : String) : ActorRef = {
		val vwstrap = new VWStrapImpl
		val vwsys = new VWSysMgrImpl
		val vwbossActorProps = Props(classOf[VWorldBossActor[VWorldSysMgr]], vwsys, vwstrap)
		val vwbActorRef : ActorRef = parentARF.actorOf(vwbossActorProps, bossActorName)
		vwbActorRef
	}
	def makeVWGoodyActor(parentARF : ActorRefFactory, goodyActorName : String, goodyCtx : BasicGoodyCtx) : ActorRef = {
		val goodyActorProps = Props(classOf[VWGoodyActor], goodyCtx)
		val goodyActorRef : ActorRef = parentARF.actorOf(goodyActorProps, goodyActorName)
		goodyActorRef
	}
	def makeVWCharAdminActor(parentARF : ActorRefFactory, chrMgrActorName : String, charMgrCtx : VWCharMgrCtx) : ActorRef = {
		val charMgrActorProps = Props(classOf[VWCharMgrActor], charMgrCtx)
		val chrMgrActorRef : ActorRef = parentARF.actorOf(charMgrActorProps, chrMgrActorName)
		chrMgrActorRef
	}
	def makeVWBodyActor(parentARF : ActorRefFactory, bodyActorName : String, dualBodyRec : DualBodyRecord) : ActorRef = {
		val bodyActorProps = Props(classOf[VWBodyActor], dualBodyRec)
		val bodyActorRef : ActorRef = parentARF.actorOf(bodyActorProps, bodyActorName)
		bodyActorRef
	}
}

