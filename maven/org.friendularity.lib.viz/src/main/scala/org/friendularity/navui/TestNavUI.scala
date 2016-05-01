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
import org.cogchar.blob.entry.EntryHost
import org.cogchar.impl.scene.read.BehavMasterConfigTest
import org.friendularity.appro.TestRaizLoad
import org.friendularity.chnkr.ChnkrWrapRepoSpec
import org.friendularity.cpump.ActorRefCPMsgTeller
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
		val nuii = new NavUiAppImpl()
 		info1("^^^^^^^^^^^^^^^^^^^^^^^^  TestNavUI.main() creted nuii={}", nuii)

		warn0("^^^^^^^^^^^^^^^^^^^^^^^^  TestNavUI.main() running detached GridSpace tst - MOVE me to a msgHandler!")
		nuii.testDetachedGS
		info0("^^^^^^^^^^^^^^^^^^^^^^^^  TestNavUI.main() - fetching legacy config graphs")
		val legConfERC_opt = nuii.getLegConfERC_opt
		info1("^^^^^^^^^^^^^^^^^^^^^^^^  TestNavUI.main() got legConfERC_opt={}", legConfERC_opt)
		nuii.sendSetupMsgs_Async
		info0("^^^^^^^^^^^^^^^^^^^^^^^^  TestNavUI.main() finished running setup msgs, now making SimSpace VWCanv")
		nuii.launchSimRenderSpace()
		info0("^^^^^^^^^^^^^^^^^^^^^^^^  TestNavUI.main() finished launchSimRenderSpace()")
		warn0("^^^^^^^^^^^^^^^^^^^^^^^^  TestNavUI.main() When user presses 'cancel' on JME splash, how can " +
					"we find that out here and exit accordingly?")
	}

}
object NavUiTestPublicNames {
	val akkaSysName : String = "NavUiStandApp_4719"
	val akkaRemotePort : Integer = 4719
	val cpumpName = "standPumpCtx_181"
}
// "App" here means FriendU app, not a JME3 "app".  The latter is made during launchSimRenderSpace at bottom.
class NavUiAppImpl extends VarargsLogging {

	lazy private val myMergedProfileJM : JenaModel = makeMergedProfileGraph("Unused selector args")

	// TODO:  Take akka setup params from profile recipes
	private val akkaSysName : String = NavUiTestPublicNames.akkaSysName
	lazy private val myAkkaSys = ActorSystem(akkaSysName)
	lazy private val myStandalonePumpSpace = new SpecialAppPumpSpace(myAkkaSys)

	private val loadLegacyConf = true
	lazy private val legConfERC_opt: Option[EnhancedLocalRepoClient] = {
		if (loadLegacyConf) Option(buildLegacyConfERC(myMergedProfileJM)) else None
	}

	lazy private val vwBossAR: ActorRef = VWorldBossFactory.makeVWorldBoss(myAkkaSys, "vworldBoss_818")
	lazy private val vwBossTeller = new ActorRefCPMsgTeller(vwBossAR)

	lazy private val standPumpTestCtxName = NavUiTestPublicNames.cpumpName
	lazy private val standPumpCtxActorRef : ActorRef = myStandalonePumpSpace.findTopActorRef(standPumpTestCtxName)
	lazy private val standPumpAdminTeller = new ActorRefCPMsgTeller(standPumpCtxActorRef)

	def getLegConfERC_opt = legConfERC_opt
	def sendSetupMsgs_Async {
		val hpatMsg = new VWARM_GreetFromPumpAdmin(standPumpAdminTeller)
		info2("Sending msg={} to VWBossTeller : {}", hpatMsg, vwBossTeller)
		vwBossTeller.tellCPMsg(hpatMsg)

		// This discovery message is usually sent from a remote client, with a more specific answerTeller
		val answerReceiver = standPumpAdminTeller
		val fptMsg = new VWARM_FindPublicTellers(answerReceiver)
		info2("Sending msg={} to VWBossTeller : {}", fptMsg, vwBossTeller)
		vwBossTeller.tellCPMsg(fptMsg)
	}
	def makeMergedProfileGraph(profileSelectorArgs : String) : JenaModel = {
		val tchunkEHost: EntryHost = TestRaizLoad.getUnitTestResourceEntryHost
		// TODO - Process the profileSelectorArgs
		val mergedProfileGraph: JenaModel = TestRaizLoad.getMergedProfileGraph_RegularDesktop(tchunkEHost)
		mergedProfileGraph
	}
	def buildLegacyConfERC(mergedProfGraph : JenaModel) : EnhancedLocalRepoClient = {
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

	def launchSimRenderSpace(): Unit = {

		val bsim = new SimBalloonAppLauncher {}
		info0("makeSimSpace Calling bsim.setup")
		bsim.setup
		info0("makeSimSpace END - vworld is now running, but delayed setup jobs may still be running/pending")
	}
	// registerAvatarConfigRepoClient(bunCtx, erc);
	def testDetachedGS : Unit = {
		val dgst = new DetachedGST{}
		dgst.gridSpaceTest
	}

}

