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
import org.cogchar.render.goody.basic.BasicGoodyCtx
import org.friendularity.appro.TestRaizLoad
import org.friendularity.cpump.{ListenChanDirectActor, CPumpMsg, CPMsgTeller, CPAdminRequestMsg}
import com.hp.hpl.jena.rdf.model.{Model => JenaModel}

/**
  * Created by Owner on 4/14/2016.
  */

// Legit state of a running VWorld system is managed by an instance of VWorldSysMgr.
trait VWorldSysMgr {
	def findPublicTellers : VWorldPublicTellers

	def findGoodyCtx : BasicGoodyCtx
}
// (hack)Strap holds any icky extra shared state that we temporarily want to pass to VWorldBossActor.
trait VWorldStrap {
//	def	getPumpCtx : DullPumpCtx
}

// The vworldBoss supplies this serializable directory of its actors to any client who asks.
// From here clients can navigate to all published vworld service actors.
// Client may also know+find these same actors and other actors by other means.
// This trait is made available as a helpful starting point, not as the definitive or exhaustive API.
trait VWorldPublicTellers extends VWorldNotice {
	def getFirstBigTeller : Option[CPMsgTeller] = None
	def getSecondCoolTeller : Option[CPMsgTeller] = None
	def getGoodyTeller : Option[CPMsgTeller] = None
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
trait VWorldBossLogic [VWSM <: VWorldSysMgr] extends VarargsLogging {
	protected def getSysMgr : VWSM

	protected def processVWorldMsg (vwmsg : VWorldRequest, localActorCtx : ActorContext): Unit = {
		val sysMgr = getSysMgr
		vwmsg match {

			case vwSetupCnfMsg : VWSetupRq_Conf => loadConf(vwSetupCnfMsg, localActorCtx)

			case vwSetupLnchMsg : VWSetupRq_Lnch => launchSimRenderSpace(vwSetupLnchMsg, localActorCtx)

			case goodyActSpecMsg : VWGoodyRqActionSpec => processVWGoodyActSpec(goodyActSpecMsg, localActorCtx)

			case goodyRdfMsg : VWGoodyRqRdf => processVWGoodyRdfMsg(goodyRdfMsg, localActorCtx)

			case adminMsg : VWAdminRqMsg => {
				// Example using the questionable "heavy" message pattern, where msg itself implements logic.
				// Using this pattern for an *admin* msg seems doubly dopey (because an inbound message from
				// client is supposed to know how to admin=rewire/reconf this server-side system - how and why?),
				// whereas if the actionSpec goody handlers worked this way, there would be some advantages.
				adminMsg.processInSys(getSysMgr, localActorCtx)
			}


		}
	}
	protected def processVWGoodyRdfMsg (goodyMsg : VWGoodyRqRdf, localActorCtx : ActorContext) : Unit = {

	}
	protected def processVWGoodyActSpec (goodyActSpecMsg : VWGoodyRqActionSpec, localActorCtx : ActorContext) : Unit = {
		val actSpec = goodyActSpecMsg.getActionSpec
		info1("VWBossLogic is processing received actSpec: {}", actSpec)
		val bgc : BasicGoodyCtx = getSysMgr.findGoodyCtx
		bgc.consumeAction(actSpec)
	}

	protected def launchSimRenderSpace(vwLnchMsg : VWSetupRq_Lnch, localActorCtx : ActorContext): Unit = {
		// TODO:  We want this launch process to call us back with the ingredients chef needs to proceed.
		val bsim = new SimBalloonAppLauncher {}
		info0("makeSimSpace Calling bsim.setup")
		bsim.setup
		info0("makeSimSpace END - vworld is now running, but delayed setup jobs may still be running/pending")
	}

	protected def loadConf(vwConfMsg : VWSetupRq_Conf, localActorCtx : ActorContext): Unit = {
		val cnfMgr = new VWCnfMgr {}
		val profileGraph = cnfMgr.getProfileGraph
		info1("Got profileGraph: {}", profileGraph)
		val legConf_opt = cnfMgr.getLegConfERC_opt
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
		case vwmsg : VWorldRequest => {
			processVWorldMsg(vwmsg, context)
		}
	}

	override protected def getSysMgr : VWSM = sysMgr

}
class VWSysMgrImpl extends VWorldSysMgr {
	override def findPublicTellers : VWorldPublicTellers = new VWorldPublicTellers{}
	// TODO:  Need access to someone who knows a BasicGoodyCtx
	override def findGoodyCtx : BasicGoodyCtx = ???
}
class VWStrapImpl extends VWorldStrap {

}
object VWorldBossFactory {
	def makeVWorldBoss(akkaSys: ActorSystem, bossActorName : String) : ActorRef = {
		val vwstrap = new VWStrapImpl
		val vwsys = new VWSysMgrImpl
		val vwbossActorProps = Props(classOf[VWorldBossActor[VWorldSysMgr]], vwsys, vwstrap)
		val vwbActorRef : ActorRef = akkaSys.actorOf(vwbossActorProps, bossActorName)
		vwbActorRef
	}
}

