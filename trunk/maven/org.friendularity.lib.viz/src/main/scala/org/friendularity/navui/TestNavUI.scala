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

import akka.actor.{ActorSystem, ActorRef}
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
import org.friendularity.cpump.{CPumpMsg, CPMsgTeller, ActorRefCPMsgTeller}
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

	// Currently URIs for all entities are assigned from *outside* the V-World (VWorldBossActor),
	// and then passed in to it via entity creation messages.   All such URIs come from one of:
	// cogchar+app ontologies, app profile data, app config chunks, or app java/scala code.


	def main(args: Array[String]): Unit = {
		// These two lines activate Log4J (at max verbosity!) without requiring a log4j.properties file.
		// However, when a log4j.properties file is present, these commands should not be used.
		//		org.apache.log4j.BasicConfigurator.configure();
		//		org.apache.log4j.Logger.getRootLogger().setLevel(org.apache.log4j.Level.ALL);
		val useOldTestApp = false
		if (useOldTestApp)
			launchGoodyRenderTestApp
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
	def launchGoodyRenderTestApp : Unit = {
		val rce: RenderConfigEmitter = new RenderConfigEmitter
		val app: GoodyRenderTestApp = new GoodyRenderTestApp(rce)
		// This will trigger the makeCogcharRenderContext() and then the simpleInitApp() below.
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

	lazy private val vwBossAR: ActorRef = VWorldBossFactory.makeVWorldBoss(myAkkaSys, "vworldBoss_818")
	lazy private val vwBossTeller = new ActorRefCPMsgTeller(vwBossAR)

	lazy private val standPumpTestCtxName = NavUiTestPublicNames.cpumpName
	lazy private val standPumpCtxActorRef : ActorRef = myStandalonePumpSpace.findTopActorRef(standPumpTestCtxName)
	lazy private val standPumpAdminTeller = new ActorRefCPMsgTeller(standPumpCtxActorRef)

	def sendSetupMsgs_Async {
		val hpatMsg = new VWARM_GreetFromPumpAdmin(standPumpAdminTeller)
		info2("Sending msg={} to VWBossTeller : {}", hpatMsg, vwBossTeller)
		vwBossTeller.tellCPMsg(hpatMsg)

		sendVWSetup_Conf()

		sendVWSetup_Lnch()

		// This discovery message is usually sent from a remote client, with a more useful answerTeller-handler
		val answerReceiver = standPumpAdminTeller
		val fptMsg = new VWARM_FindPublicTellers(answerReceiver)
		info2("Sending msg={} to VWBossTeller : {}", fptMsg, vwBossTeller)
		vwBossTeller.tellCPMsg(fptMsg)

		sndGoodyTstMsgs
	}
	def sendVWSetup_Conf() : Unit = {
		val msg = new VWSetupRq_Conf
		vwBossTeller.tellCPMsg(msg)
	}

	def sendVWSetup_Lnch() : Unit = {
		val msg = new VWSetupRq_Lnch
		vwBossTeller.tellCPMsg(msg)
	}

	import scala.collection.JavaConverters._
	def sndGoodyTstMsgs: Unit = {

		val gtmm: GoodyTestMsgMaker = new GoodyTestMsgMaker
		val msgsJList = gtmm.makeGoodyCreationMsgs
		val msgsScbuf = msgsJList.asScala
		for (actSpec <- msgsScbuf) {
			getLogger.info("Wrapping and sending: {}", actSpec)
			val vwMsgWrap = new VWGoodyRqBTAS(actSpec)
			vwBossTeller.tellCPMsg(vwMsgWrap)
		}
	}
	def middleRoad : Unit = {

	}
	def testDetachedGS : Unit = {
		val dgst = new DetachedGST{}
		dgst.gridSpaceTest
	}
	// registerAvatarConfigRepoClient(bunCtx, erc);
}
/*
class SetupResultsRcvr extends  CPMsgTeller with VarargsLogging {
	override def tellCPMsg(msg: CPumpMsg): Unit = {
		info1("Got setup results: {}", msg)
	}
}
*/