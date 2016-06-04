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

// import akka.actor._
import akka.actor.{Actor, ActorRef, ActorContext, ActorSystem, ActorRefFactory, Props, ActorLogging}
import com.hp.hpl.jena.rdf.model.{Model => JenaModel}
import org.appdapter.core.store.Repo
import org.appdapter.fancy.log.VarargsLogging
import org.appdapter.fancy.rclient.EnhancedLocalRepoClient
import org.cogchar.blob.emit.RenderConfigEmitter
import org.cogchar.blob.entry.EntryHost
import org.cogchar.impl.scene.read.BehavMasterConfigTest
import org.cogchar.impl.thing.basic.BasicThingActionSpec
import org.cogchar.render.rendtest.{GoodyTestMsgMaker, GoodyRenderTestApp}
import org.friendularity.appro.TestRaizLoad
import org.friendularity.chnkr.ChnkrWrapRepoSpec
import org.friendularity.cpump.{CPStrongTeller, CPumpMsg, CPMsgTeller, ActorRefCPMsgTeller}
import org.friendularity.dull.SpecialAppPumpSpace

import org.friendularity.respire._

/**
  * Created by Owner on 4/1/2016.
  */
object TestNavUI extends VarargsLogging {

	// Goal - load vworld *incrementally* using messages found in "modern" config chunk(s),
	// mediated by higher-level instructions from profile recipes.  Most of these messages
	// (other than gross system-startup and system-shutdown) should be conveyable over
	// network, so the load ordering logic is independent from the instruction execution.

	// From the outside, the VWorld entities are all identified *only* by URI (optionally
	// extended by offset params).  Anything not identified by URI(+offset) must be private
	// to the VWorld.

	// Currently URIs for all entities are assigned from *outside* the V-World (boundary defined by VWorldBossActor),
	// and then passed in to it via entity creation messages (sent to VWBossActor and its descendants).   All such
	// URIs come from one of these places:
	//  cogchar+app ontologies, app profile data, app config chunks, or app java/scala code.


	def main(args: Array[String]): Unit = {
		// These two lines activate Log4J (at max verbosity!) without requiring a log4j.properties file.
		// However, when a log4j.properties file is present, these commands should not be used.
		//		org.apache.log4j.BasicConfigurator.configure();
		//		org.apache.log4j.Logger.getRootLogger().setLevel(org.apache.log4j.Level.ALL);
		val useOldTestApp = false
		if (useOldTestApp)
			launchOldGoodyRenderTestApp
		else
			launchNuiiTest
	}
	def launchNuiiTest : Unit = {
		val nuii = new NavUiAppImpl()
 		info1("^^^^^^^^^^^^^^^^^^^^^^^^  TestNavUI.main() created nuii={}", nuii)
		warn0("^^^^^^^^^^^^^^^^^^^^^^^^  TestNavUI.main() running detached GridSpace tst - MOVE me to a msgHandler!")
		nuii.testDetachedGS
		// info0("^^^^^^^^^^^^^^^^^^^^^^^^  TestNavUI.main() - fetching legacy config graphs")
		// val legConfERC_opt = nuii.getLegConfERC_opt
		// info1("^^^^^^^^^^^^^^^^^^^^^^^^  TestNavUI.main() got legConfERC_opt={}", legConfERC_opt)
		nuii.sendSetupMsgs_Async
		// info0("^^^^^^^^^^^^^^^^^^^^^^^^  TestNavUI.main() finished running setup msgs, now making SimSpace VWCanv")
		// nuii.launchSimRenderSpace()
		//info0("^^^^^^^^^^^^^^^^^^^^^^^^  TestNavUI.main() finished launchSimRenderSpace()")
		warn0("^^^^^^^^^^^^^^^^^^^^^^^^  TestNavUI.main() When user presses 'cancel' on JME splash, how can " +
					"we find that out here and exit accordingly?")

	}
	private def launchOldGoodyRenderTestApp : Unit = {
		// Just a wrapper for the same test we can run inside Cogchar o.c.lib.render.goody.
		// Does not use any FriendU code.
		// This is just a classpath sanity test.
		// Normally we run the Nuii test above, instead, and this code is unused.
		val rce: RenderConfigEmitter = new RenderConfigEmitter
		val app: GoodyRenderTestApp = new GoodyRenderTestApp(rce)
		app.start
	}
}
object NavUiTestPublicNames {
	// TODO:  Push as many setup params as possible downward, feed them from profile recipes
	val akkaSysName : String = "NavUiStandApp_4719"
	val akkaRemotePort : Integer = 4719
	val cpumpName = "standPumpCtx_181"
}
// "App" here means FriendU app, not a JME3 "app".  The latter is made during launchSimRenderSpace at bottom.
class NavUiAppImpl extends VarargsLogging {

	private val akkaSysName : String = NavUiTestPublicNames.akkaSysName
	lazy private val myAkkaSys = ActorSystem(akkaSysName)
	lazy private val myStandalonePumpSpace = new SpecialAppPumpSpace(myAkkaSys)

	lazy private val vwBossAR: ActorRef = VWorldActorFactoryFuncs.makeVWorldBoss(myAkkaSys, "vworldBoss_818")
	lazy private val vwBossTeller = new ActorRefCPMsgTeller(vwBossAR)

	lazy private val standPumpTestCtxName = NavUiTestPublicNames.cpumpName
	lazy private val standPumpCtxActorRef : ActorRef = myStandalonePumpSpace.findTopActorRef(standPumpTestCtxName)
	lazy private val standPumpAdminTeller = new ActorRefCPMsgTeller(standPumpCtxActorRef)

	// Direct approach
	lazy private val outerCtxName = "nav_ui_outer"
	lazy private val outerCtxActorRef : ActorRef = makeCustomOuterActor(myAkkaSys)
	lazy private val outerTellerDirect = new ActorRefCPMsgTeller(outerCtxActorRef)


	// Jobby approach
	lazy private val jobbyOuterTeller = OuterJobbyLogic_MasterFactory.makeOoLogicAndTeller(myAkkaSys)

	// Custom-outer approach
	private def makeCustomOuterActor(akkaSys: ActorSystem) : ActorRef = {
		val vwbossActorProps = Props(classOf[OuterDirectActor])
		val vwbActorRef : ActorRef = akkaSys.actorOf(vwbossActorProps, outerCtxName)
		vwbActorRef
	}

	def sendSetupMsgs_Async {
		val hpatMsg = new VWARM_GreetFromPumpAdmin(standPumpAdminTeller)
		info2("Sending msg={} to VWBossTeller : {}", hpatMsg, vwBossTeller)
		vwBossTeller.tellCPMsg(hpatMsg)

		sendVWSetup_Conf()

		sendVWSetup_Lnch()

		// Request the results of happy startup, to trigger further ops
		val fptMsg = new VWARM_FindPublicTellers(jobbyOuterTeller)
		info2("Sending msg={} to VWBossTeller : {}", fptMsg, vwBossTeller)
		vwBossTeller.tellCPMsg(fptMsg)

	}
	def sendVWSetup_Conf() : Unit = {
		val msg = new VWSetupRq_Conf
		vwBossTeller.tellCPMsg(msg)
	}

	def sendVWSetup_Lnch() : Unit = {
		val msg = new VWSetupRq_Lnch
		vwBossTeller.tellCPMsg(msg)
	}

	def testDetachedGS : Unit = {
		val dgst = new DetachedGST{}
		dgst.gridSpaceTest
	}
	// registerAvatarConfigRepoClient(bunCtx, erc);
}

trait NavUiOuterLogic extends VarargsLogging {
	def rcvPubTellers (vwpt : VWorldPublicTellers): Unit = {
		// This is the notice we get back from boss, confirming system has started and these tellers are ready for biz.
		info1("Outer logic got public tellers: {}", vwpt)
		val goodyTeller = vwpt.getGoodyTeller
		if (goodyTeller.isDefined) {
			info1("Sending goody tst msgs to: {}", goodyTeller.get)
			sndGoodyTstMsgs(goodyTeller.get)
		} else {
			warn0("GoodyTeller is not available, cannot send goody tst msgs.")
		}

	}
	import scala.collection.JavaConverters._
	def sndGoodyTstMsgs(goodyTeller : CPMsgTeller): Unit = {

		val gtmm: GoodyTestMsgMaker = new GoodyTestMsgMaker
		val msgsJList = gtmm.makeGoodyCreationMsgs
		val msgsScbuf = msgsJList.asScala
		for (actSpec <- msgsScbuf) {
			getLogger.info("Wrapping and sending: {}", actSpec)
			val vwMsgWrap = new VWGoodyRqBTAS(actSpec)
			goodyTeller.tellCPMsg(vwMsgWrap)
		}
	}

}
//
// If this class were going to take constructor params, we would have to pass those through the Props mechanism.
class OuterDirectActor	extends Actor with ActorLogging with NavUiOuterLogic {
	def receive = {
		case vwpt : VWorldPublicTellers => rcvPubTellers(vwpt)
	}
}

// Unnecessary to use the Jobby approach here, but working through it anyway as an excercise.
// First we must define the actual job-handler classes.
// Give the logic a dummy constructor param just to show how they flow in.
class OuterJobbyLogic(unusedParam : Int) extends MsgJobLogic[VWorldPublicTellers] with NavUiOuterLogic {
	// Differences here is that we get exception handling+logging, runtime type verification,
	// and actor wrapping for free, but we must also create the factory stuff below.
	// Note that we could also pass constructor parameters in via the factory, without Props hassles.
	override def processMsgUnsafe(msg : VWorldPublicTellers, slf : ActorRef, sndr : ActorRef,
								  actx : ActorContext) : Unit = {
		info1("Processing jobby version of public tellers handler, unusedParam={}", unusedParam : Integer)
		rcvPubTellers(msg)
	}
}
// Now we would make the factories needed to construct our logic + actor instances.
// Question is:  When is this less bother than making an actor wrapper by hand, as in OuterDirectActor above?

object OuterJobbyLogic_MasterFactory extends VWorldMasterFactory {
	val thatOtherParam : Int = 22
	val oolFactory = new MsgJobLogicFactory[VWorldPublicTellers]() {
		override def makeJobLogic(msgFilterClz: Class[VWorldPublicTellers]): MsgJobLogic[VWorldPublicTellers] = {
			info1("Making outer jobby logic for specific runtime filter clz: {}", msgFilterClz)
			new OuterJobbyLogic(thatOtherParam)
		}
	}
	val oolFactPair = makeFactoryPair[VWorldPublicTellers](classOf[VWorldPublicTellers], oolFactory)

	val oolJobbyActorName = "outer_jobby"

	def makeOoLogicAndTeller(arf : ActorRefFactory) : CPStrongTeller[VWorldPublicTellers] = {
		val aref = oolFactPair.makeLogicAndActor(arf, oolJobbyActorName, None)
		new ActorRefCPMsgTeller[VWorldPublicTellers](aref)
	}
}