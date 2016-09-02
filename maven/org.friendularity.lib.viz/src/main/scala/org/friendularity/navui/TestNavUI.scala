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
import akka.actor.{Scheduler => AkkaSched, Actor, ActorRef, ActorContext, ActorSystem, ActorRefFactory, Props, ActorLogging}
import com.hp.hpl.jena.rdf.model.{Model => JenaModel}
import com.jme3.math.Vector3f
import org.appdapter.core.name.{FreeIdent, Ident}
import org.appdapter.core.store.Repo
import org.appdapter.fancy.log.VarargsLogging
import org.appdapter.fancy.rclient.EnhancedLocalRepoClient
import org.cogchar.api.humanoid.HumanoidFigureConfig
import org.cogchar.api.vworld.GoodyActionParamWriter
import org.cogchar.bind.mio.robot.svc.ModelBlendingRobotServiceContext
import org.cogchar.blob.emit.RenderConfigEmitter
import org.cogchar.blob.entry.EntryHost
import org.cogchar.impl.scene.read.BehavMasterConfigTest
import org.cogchar.impl.thing.basic.{BasicTypedValueMap, BasicThingActionSpec}
import org.cogchar.impl.thing.fancy.ConcreteTVM
import org.cogchar.name.goody.GoodyNames
import org.cogchar.platform.gui.keybind.KeyBindingConfig
import org.cogchar.platform.trigger.CommandSpace
import org.cogchar.render.app.humanoid.HumanoidRenderWorldMapper
import org.cogchar.render.goody.basic.BasicGoodyCtx
import org.cogchar.render.model.humanoid.{VWorldHumanoidFigureEntity, HumanoidFigure}
import org.cogchar.render.rendtest.{GoodyTestMsgMaker, GoodyRenderTestApp}
import org.cogchar.render.sys.goody.GoodyRenderRegistryClient
import org.friendularity.netcli.vwta.ClientTestMsgSender
import org.friendularity.qpc.OffersVWorldClient
import org.friendularity.raiz.{VizappLegacyLoaderFactory, VizappProfileLoaderFactory, TestRaizLoad}
import org.friendularity.vwimpl.IdentHlp
import org.friendularity.vwmsg.{PartialTransform3D, MaybeTransform3D, VWTAMsgMaker}

/**
  * Created by Stub22 on 4/1/2016.
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
		val navUiAppImpl : NavUiAppImpl = appSysStandalone.findOrMakeNavUiApp
		val navUiAppSvc : NavUiAppSvc = navUiAppImpl
 		info1("^^^^^^^^^^^^^^^^^^^^^^^^  TestNavUI.main() created nuii={}", navUiAppImpl)
		warn0("^^^^^^^^^^^^^^^^^^^^^^^^  TestNavUI.main() running detached GridSpace tst - MOVE me to a msgHandler!")
		navUiAppImpl.testDetachedGS
		// info0("^^^^^^^^^^^^^^^^^^^^^^^^  TestNavUI.main() - fetching legacy config graphs")
		// val legConfERC_opt = nuii.getLegConfERC_opt
		// info1("^^^^^^^^^^^^^^^^^^^^^^^^  TestNavUI.main() got legConfERC_opt={}", legConfERC_opt)
		navUiAppImpl.sendSetupMsgs_Async

		info0("========== TestNavUI.main() starting VW-SERVER qpidConn")
		navUiAppImpl.startQpidConn
		navUiAppImpl.checkServerSvcs()

		maybeLaunchPhonyClient
		val flag_sendTestMovesFromExoUserLogic = true
		val bodyUserLogic = navUiAppSvc.makeFunUserLogic(flag_sendTestMovesFromExoUserLogic)
		appSysStandalone.sendStart_SemiLegacySinbad(bodyUserLogic)


		// info0("^^^^^^^^^^^^^^^^^^^^^^^^  TestNavUI.main() finished running setup msgs, now making SimSpace VWCanv")
		// nuii.launchSimRenderSpace()
		//info0("^^^^^^^^^^^^^^^^^^^^^^^^  TestNavUI.main() finished launchSimRenderSpace()")
		warn0("^^^^^^^^^^^^^^^^^^^^^^^^  TestNavUI.main() When user presses 'cancel' on JME splash, how can " +
					"we find that out here and exit accordingly?")

	}
	val myFlag_addPhonyClient = true
	private def maybeLaunchPhonyClient: Unit = {
		if (myFlag_addPhonyClient) {
			val (doSinbadMoves, doExtraCam, doGoodyPile) = (true, true, true)
			val clientTestSender = new ClientTestMsgSender(3000, 2000, doSinbadMoves, doExtraCam, doGoodyPile)
			clientTestSender.startTestThread
		}
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
	private val akkaSysName: String = NavUiTestPublicNames.akkaSysName
	lazy private val myAkkaSys = ActorSystem(akkaSysName)
	lazy private val myNavUiApp = new NavUiAppImpl(myAkkaSys)

	def findOrMakeNavUiApp: NavUiAppImpl = myNavUiApp

	lazy private val myProfileLoader = VizappProfileLoaderFactory.makeUnitTestProfileLoader()

	lazy private val myMergedProfileGraph : JenaModel = myProfileLoader.makeMergedProfileGraph

	lazy private val myLegacyLoader = VizappLegacyLoaderFactory.makeUnitTestLegacyLoader()

	lazy private val myLegacyELRC: EnhancedLocalRepoClient = myLegacyLoader.makeLegacyELRC(myMergedProfileGraph)
	/*
	{
		// Note that TestRaizLoad extends AvatarLegacySetupFuncs, and is also currently used from CCMIO_DemoActivator
		val setupLoader = TestRaizLoad.getDfltSetupLoader
		val unitTestConfEHost: EntryHost = setupLoader.getUnitTestResourceEntryHost
		val mergedProfileGraph: JenaModel = setupLoader.getMergedProfileGraph_RegularDesktop(unitTestConfEHost)
		val legConfEHost: EntryHost = unitTestConfEHost // just emphasizing this is a separate choice
		val vzBrkRcpUriTxt: String = setupLoader.myRootNames.vzpLegCnfBrkrRcpUriTxt
		setupLoader.makeLegacyConfigELRC_fromJena(mergedProfileGraph, vzBrkRcpUriTxt, legConfEHost)
	}
	*/
	def getLegacyELRC: EnhancedLocalRepoClient = myLegacyELRC

	def sendStart_SemiLegacySinbad(ebul: ExoBodyUserLogic): Unit = {
		myNavUiApp.requestStandySemiLegacyBody_Sinbad(myAkkaSys, myLegacyELRC, ebul)
	}

	def harumph(kbc : KeyBindingConfig, cspace: CommandSpace, bgc: BasicGoodyCtx): Unit = {

	}
}
/*
22966   INFO [Service Manager Thread - 7] org.cogchar.platform.gui.keybind.KeyBindingConfig
(KeyBindingConfig.java:53) addBindings - addBindings found 42 bindings

		// PumaVirtualWorldMapper.initVirtualWorlds   calls
		// 	        hrc.refreshInputBindingsAndHelpScreen(currKeyBindCfg, cspace);
		// HumanoidRenderContext does:
	//	public void refreshInputBindingsAndHelpScreen(KeyBindingConfig keyBindConfig, CommandSpace cspace) {
	//		RenderRegistryClient rrc = getRenderRegistryClient();
	//		VW_InputBindingFuncs.setupKeyBindingsAndHelpScreen(rrc, keyBindConfig, getAppStub(),
	//			getJMonkeyAppSettings(), cspace);
	//	}
VW_InputBindingFuncs does:

static private VW_InputDirector theOpenGLInputDirector;
	public static void setupKeyBindingsAndHelpScreen(final RenderRegistryClient rrc, KeyBindingConfig keyBindConfig,
					WorkaroundAppStub appStub, AppSettings someSettings, CommandSpace cspace) {
		if (theOpenGLInputDirector == null) {
			theOpenGLInputDirector = new VW_InputDirector();
		}

		theOpenGLInputDirector.myRenderRegCli = rrc;
		theOpenGLInputDirector.myKeyBindCfg = keyBindConfig;
		theOpenGLInputDirector.myAppStub = appStub;
	//	theOpenGLInputDirector.myHRC_elim = hrc;
		theOpenGLInputDirector.myAppSettings = someSettings;

		theOpenGLInputDirector.myCommandSpace = cspace;

		theOpenGLInputDirector.clearKeyBindingsAndHelpScreen();

		theOpenGLInputDirector.setupKeyBindingsAndHelpScreen();
-----------------------------------------------
VW_InputDirector
    public void setupKeyBindingsAndHelpScreen() {
        InputManagerDoodad doodad = new InputManagerDoodad();
        doodad.myJME3InputManager = myRenderRegCli.getJme3InputManager(null);
        // If we do that, we'd better clear the KeyBindingTracker too
        // Since we just cleared mappings and are (for now at least) using the default FlyByCamera mappings, we must re-register them
        FlyByCamera fbCam = myAppStub.getFlyByCamera();
        fbCam.registerWithInput(doodad.myJME3InputManager);
        // Now we'll register the mappings in Cog Char based on theConfig
        // HumanoidPuppetActions.setupActionListeners(inputManager, myHRC_elim, myKeyBindCfg, myKeyBindingTracker);
        setupActionListeners(doodad, myKeyBindCfg, myKeyBindingTracker);
        addScrollWheelBindings(doodad);
        setupCommandKeybindings();

        // ... and finally set up the help screen now that the mappings are done
        myHelpScreenMgr.updateHelpTextContents(myRenderRegCli, myAppSettings, myKeyBindCfg, myKeyBindingTracker);
    }
		// val hrwMapper: HumanoidRenderWorldMapper = new HumanoidRenderWorldMapper
		// hrwMapper.addHumanoidGoodies(bgc, hrc)
//		val grrc: GoodyRenderRegistryClient = hrc.getGoodyRenderRegistryClient
// 		hrc.refreshInputBindingsAndHelpScreen(currKeyBindCfg, cspace)
//		val humanoidFigures: util.Map[Ident, HumanoidFigure] = hrc.getHumanoidFigureManager.getHumanoidFigures
//		import scala.collection.JavaConversions._
//		for (figureUri <- humanoidFigures.keySet) {
//			theLogger.info("Adding a HumanoidFigureGoodyWrapper for {}", figureUri)
//			val figure: HumanoidFigure = humanoidFigures.get(figureUri)
//			val vhfe: VWorldHumanoidFigureEntity = new VWorldHumanoidFigureEntity(grrc, figureUri, figure)
//			bgc.getVWER.addGoody(vhfe)
//		}
*/


