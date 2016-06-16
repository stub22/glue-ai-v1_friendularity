package org.friendularity.respire

import akka.actor.{ActorContext, ActorRef}
import com.jme3.asset.AssetManager
import com.jme3.input.FlyByCamera
import com.jme3.math.ColorRGBA
import com.jme3.renderer.ViewPort
import com.jme3.scene.{Node => JmeNode}
import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.api.space.{GridSpaceFactory, MultiDimGridSpace, CellRangeFactory}
import org.cogchar.render.sys.goody.{GoodyRenderRegistryClient, GoodyModularRenderContext}
import org.cogchar.render.sys.window.WindowStatusMonitor
import org.friendularity.cpump.CPMsgTeller


import org.cogchar.bind.midi.in.{CCParamRouter, TempMidiBridge}
import org.cogchar.render.sys.context.{PhysicalModularRenderContext, FramedRenderContext, CogcharRenderContext}
import org.cogchar.render.sys.registry.RenderRegistryClient
import org.cogchar.render.trial.{TrialUpdater, TrialCameras, TrialContent, TrialBalloon}
import org.friendularity.vwmsg.{FullIngredMsgImpl, VWSetupResultsNotice, VWSceneCoreRq}

// Marker trait for a vw sys kernel.   Usually only one exists per java runtime, but should be safe in plural, too.
trait VWCore {

}
trait VWSceneGraphMgr extends VWorldJobLogic[VWSceneCoreRq] {
	protected def getRootNode
	// "Unsafe" means we are allowed to throw any exceptions, whee!
	override def processMsgUnsafe(msg : VWSceneCoreRq, slf : ActorRef, sndr : ActorRef, actx : ActorContext) : Unit = {
		// msg.processInsideUnsafe(this, slf, sndr, actx)
		info1("Processing scene core rq: {}", msg)

	}

}
trait SimBalloonAppLauncher extends VarargsLogging {
	lazy val mySBApp: SimBalloonJmeApp = new SimBalloonJmeApp
	// Generally called on main() thread to do initial app setup.
	def setup(resultsTeller : CPMsgTeller) : Unit = {
		mySBApp.wireSetupResultsTeller(resultsTeller)
		// Code copied from TrialBalloon.main
		info0("^^^^^^^^^^^^^^^^^^^^^^^^  SimBalloonAppLauncher.setup() calling initMidi()")
		// Initialize any MIDI stuff, so it is available during app wiring, if desired.
		mySBApp.initMidi
		info0("^^^^^^^^^^^^^^^^^^^^^^^^  SimBalloonAppLauncher.setup() calling JME3 start(), which will in turn call TrialBalloon.simpleInitApp()")
		// Start the JME3 Virtual world.
		// We may be blocked in this method, showing splash screen, until the user confirms Canvas startup.
		mySBApp.start  // Eventually calls SimBalloonJmeApp.doMoreSimpleInit, below.

		// org.cogchar.api.space.GridSpaceTest.goGoGo
		// info0("^^^^^^^^^^^^^^^^^^^^^^^^  SimBalloonLauncher.setup() starting config-load test")
		// app.optLoadConfig();
	//	info0("^^^^^^^^^^^^^^^^^^^^^^^^ SimBalloonLauncher.setup() calling playMidiOutput()")
	//	tbApp.playMidiOutput

		info0("^^^^^^^^^^^^^^^^^^^^^^^^ End of SimBalloonAppLauncher.setup()")
	}

}
trait UpdateAttacher {
	def attachUpdater(tu : TrialUpdater) : Unit
}
// This is our "app" class in the JME taxonomy.
// It *inherits* a .start() method, and init callback, which eventually calls doMoreSimpleInit.
// It also inherits an enqueue() method that can be used to postpone work into a later task on render thread.
// We use that capability to schedule our real init work.
class SimBalloonJmeApp extends BigBalloonJmeApp with UpdateAttacher with VWCore {
	var myResultsTeller_opt : Option[CPMsgTeller] = None
	def wireSetupResultsTeller(resultsTeller : CPMsgTeller): Unit = {
		myResultsTeller_opt = Some(resultsTeller)
	}
	override def attachUpdater(tu : TrialUpdater) : Unit = {
		attachVWorldUpdater(tu)
	}
	// Invoked on JME3 app-init thread, as part of simpleAppInit,
	// initiated by mySBApp.start above.
	// Creates a sampleContentTask object to do our remaining setup *later*, queued
	// to JME thread.
	override protected def doMoreSimpleInit: Unit = {
		// Make and launch a task for later builtin (hardcoded) setup of this app, to be run directly on render
		// thread. Best to make it a smaller amount of stuff, and do more population later, based on
		// a series of smaller client requests, which may be read by an outer config mechanism when desired.
		// In that case the render-threading is in smaller batches.

		val sampleContentTask = makeSampleContentCreationTask // Make description of work to be done later.

		// Define and instantiate callback object.
		val scTskCallable = new java.util.concurrent.Callable[Unit] {
			@throws(classOf[Exception])	override def call : Unit = {
				// One big chunk of work, done on the render thread.
				sampleContentTask.doItMostEasily
				if (myResultsTeller_opt.isDefined) {
					val notice = makeSetupResultsNotice()
					getLogger.info("Sending setupResults notice: {}", notice)
					myResultsTeller_opt.get.tellCPMsg(notice)
				} else {
					getLogger().warn("No results teller found, so no setup results are passed back.")
				}
			}
		}
		// Now the sampleContent task is made and ready to enqueue.
		// Defer the callable work onto a future callback on render-thread, which occurs after JME.start is complete
		// (which can only happen after this method we are in returns!)
		getLogger().info("Deferring additional VWorld setup to future rend-thread task: {}", scTskCallable)
		val futureWhichWeIgnore = enqueue(scTskCallable)
	}

	// Make (not serializable) notice sent to our results teller after the OpenGL
	// world is running, allowing the app to confidently perform any further wiring.

	private def makeSetupResultsNotice(): VWSetupResultsNotice = {
		// Limitation:  These yummy ingred are usually not serializable.
		val crc = getRenderContext
		val framedRendCtx : FramedRenderContext = crc.asInstanceOf[FramedRenderContext]
		val winStatMon : WindowStatusMonitor = framedRendCtx
		val rrc : RenderRegistryClient = crc.getRenderRegistryClient
		val physModRendCtx = crc.asInstanceOf[PhysicalModularRenderContext]
		// We currently happen to lump all the ingredients together, but we have the choice
		// to instead provide finer grained LesserIngred and BodyMgrIngred.
		val fullIng = new FullIngredMsgImpl(rrc, winStatMon, physModRendCtx)
		val notice = new VWSetupResultsNotice(fullIng, fullIng)
		notice
	}
	private def makeSampleContentCreationTask : MoreIsolatedBonusContentTask = {
		val crc: CogcharRenderContext = getRenderContext
		// TODO:  Get the flyCam and viewPort from render context/registry stuff, instead of here in the "app".
		val fc : FlyByCamera = getFlyByCamera(); // Defined in CogcharPresumedApp
		val vp : ViewPort = getPrimaryAppViewPort(); // Defined in CogcharPresumedApp
		val miit = new MoreIsolatedBonusContentTask(crc, this, myTMB, fc, vp)
		miit
	}

	override def destroy : Unit = {
		getLogger.warn("SimBalloonJmeApp.destroy() called, indicating JME app exit.")
		getLogger.error("TODO:  Send PoisonPill to actorSystem, so rest of application can exit.")
		getLogger.info("Now calling super.destroy().")
		super.destroy
	}
}
