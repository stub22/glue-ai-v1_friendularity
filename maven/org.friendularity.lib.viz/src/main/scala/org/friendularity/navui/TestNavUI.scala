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
import org.appdapter.core.name.Ident
import org.appdapter.core.store.Repo
import org.appdapter.fancy.log.VarargsLogging
import org.appdapter.fancy.rclient.EnhancedLocalRepoClient
import org.cogchar.api.humanoid.HumanoidFigureConfig
import org.cogchar.bind.mio.robot.svc.ModelBlendingRobotServiceContext
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
		val appSysStandalone = new StandaloneNavAppSys();
		val nuii = appSysStandalone.findOrMakeNavUiApp
 		info1("^^^^^^^^^^^^^^^^^^^^^^^^  TestNavUI.main() created nuii={}", nuii)
		warn0("^^^^^^^^^^^^^^^^^^^^^^^^  TestNavUI.main() running detached GridSpace tst - MOVE me to a msgHandler!")
		nuii.testDetachedGS
		// info0("^^^^^^^^^^^^^^^^^^^^^^^^  TestNavUI.main() - fetching legacy config graphs")
		// val legConfERC_opt = nuii.getLegConfERC_opt
		// info1("^^^^^^^^^^^^^^^^^^^^^^^^  TestNavUI.main() got legConfERC_opt={}", legConfERC_opt)
		nuii.sendSetupMsgs_Async

		appSysStandalone.sendStart_SemiLegacySinbad()

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
// Use to run from main().
class StandaloneNavAppSys() {
	private val akkaSysName : String = NavUiTestPublicNames.akkaSysName
	lazy private val myAkkaSys = ActorSystem(akkaSysName)
	lazy private val myNavUiApp = new NavUiAppImpl(myAkkaSys)

	def findOrMakeNavUiApp : NavUiAppImpl = myNavUiApp

	lazy private val myLegacyELRC: EnhancedLocalRepoClient = {

		// Note that TestRaizLoad extends AvatarLegacySetupFuncs, and is also currently used from CCMIO_DemoActivator
		val unitTestConfEHost: EntryHost = TestRaizLoad.getUnitTestResourceEntryHost
		val mergedProfileGraph: JenaModel = TestRaizLoad.getMergedProfileGraph_RegularDesktop(unitTestConfEHost)
		val legConfEHost: EntryHost = unitTestConfEHost // just emphasizing this is a separate choice
		val vzBrkRcpUriTxt: String = TestRaizLoad.vzpLegCnfBrkrRcpUriTxt
		TestRaizLoad.makeAvatarLegacyConfigRepo(mergedProfileGraph, vzBrkRcpUriTxt, legConfEHost)
	}

	def getLegacyELRC : EnhancedLocalRepoClient = myLegacyELRC

	def sendStart_SemiLegacySinbad(): Unit = {
		myNavUiApp.requestStandySemiLegacyBody_Sinbad(myAkkaSys, myLegacyELRC)
	}
}


