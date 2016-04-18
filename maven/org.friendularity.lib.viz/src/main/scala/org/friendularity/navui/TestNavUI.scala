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
import com.hp.hpl.jena.rdf.model.Model
import org.appdapter.core.store.Repo
import org.appdapter.fancy.log.VarargsLogging
import org.appdapter.fancy.rclient.EnhancedLocalRepoClient
import org.cogchar.blob.entry.EntryHost
import org.cogchar.impl.scene.read.BehavMasterConfigTest
import org.friendularity.appro.TestRaizLoad
import org.friendularity.chnkr.ChnkrWrapRepoSpec
import org.friendularity.cpump.ActorRefCPMsgTeller
import org.friendularity.dull.SpecialAppPumpSpace


import org.friendularity.respire.{VWARM_FindPublicTellers, VWARM_GreetFromPumpAdmin, VWorldBossFactory}

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

		val nuii = new NavUiAppImpl()
 		info1("^^^^^^^^^^^^^^^^^^^^^^^^  TestNavUI.main() creted nuii={}", nuii)
		nuii.runSetupMsgs
	}

}


class NavUiAppImpl extends VarargsLogging {
	private val akkaSysName = "NavUiStandApp_4719"
	lazy private val myAkkaSys = ActorSystem(akkaSysName)
	lazy private val myStandalonePumpSpace = new SpecialAppPumpSpace(myAkkaSys)
	private val loadLegacyConf = true

	lazy private val legConfERC_opt: Option[EnhancedLocalRepoClient] = if (loadLegacyConf) Option(buildLegacyConfERC) else None

	lazy private val vwBossAR: ActorRef = VWorldBossFactory.makeVWorldBoss(myAkkaSys, "vworldBoss_818")
	lazy private val vwBossTeller = new ActorRefCPMsgTeller(vwBossAR)

	lazy private val standPumpTestCtxName = "standPumpCtx_181"
	lazy private val standPumpCtxActorRef : ActorRef = myStandalonePumpSpace.findTopActorRef(standPumpTestCtxName)
	lazy private val standPumpAdminTeller = new ActorRefCPMsgTeller(standPumpCtxActorRef)

	def runSetupMsgs {
		val hpatMsg = new VWARM_GreetFromPumpAdmin(standPumpAdminTeller)
		vwBossTeller.tellCPMsg(hpatMsg)
		info1("HelloPumpAdminTeller SENT to VWBossTeller : {}", vwBossTeller)
		val fptMsg = new VWARM_FindPublicTellers(standPumpAdminTeller)
		vwBossTeller.tellCPMsg(fptMsg)
		info1("VWARM_FindPublicTellers SENT to VWBossTeller : {}", vwBossTeller)
	}

	def buildLegacyConfERC : EnhancedLocalRepoClient = {
		// Legacy config load section, gradually becoming obsolete:
		// Under OSGi (e.g. CCMIO), old PumaBoot process is set up by attachVizTChunkLegConfRepo(BundleContext bunCtx).
		// But here, we only need the actor boundary between the client file data config side, and the server VWorld.

		val tchunkEHost: EntryHost = TestRaizLoad.makeBundleEntryHost(TestRaizLoad.getClass)
		val mergedProfileGraph: Model = TestRaizLoad.getMergedProfileGraph_RegularDesktop(tchunkEHost)
		val vzBrkRcpUriTxt: String = TestRaizLoad.vizappBrokerRecipeUriTxt

		val legConfERC = TestRaizLoad.makeAvatarLegacyConfigRepo(mergedProfileGraph, vzBrkRcpUriTxt, tchunkEHost)
		getLogger.info("legConfERC={}", legConfERC)
		legConfERC
	}


	// registerAvatarConfigRepoClient(bunCtx, erc);

}






