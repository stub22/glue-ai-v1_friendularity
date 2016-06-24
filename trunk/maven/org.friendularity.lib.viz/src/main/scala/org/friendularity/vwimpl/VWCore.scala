package org.friendularity.vwimpl

import akka.actor.{ActorContext, ActorRef}
import com.jme3.input.FlyByCamera
import com.jme3.renderer.ViewPort
import com.jme3.scene.{Node => JmeNode}
import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.render.goody.dynamic.DynamicGoodyParent
import org.cogchar.render.sys.context.{CogcharRenderContext, FramedRenderContext, PhysicalModularRenderContext}
import org.cogchar.render.sys.registry.RenderRegistryClient
import org.cogchar.render.sys.window.WindowStatusMonitor
import org.cogchar.render.trial.{TrialBalloon, TrialUpdater}
import org.friendularity.cpump.CPMsgTeller
import org.friendularity.respire.SweetDynaSpace
import org.friendularity.vwmsg.{FullIngredMsgImpl, VWSceneCoreRq, VWSetupResultsNotice}

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
trait UpdateAttacher {  // Is implemented by:  SimBalloonJmeApp (in VWCore.scala)
	// Note that TrialContent implements TrialUpdater
	def attachUpdater(tu : TrialUpdater) : Unit
}
// Binds sweetDynaSpace to vworld
class ArbBalloonJmeApp extends TrialBalloon with DynamicGoodyParent {

	def attachDeepDynaSpace(sweetDS: SweetDynaSpace) {
		sweetDS.setParent(this)
		// Attaches the space for callbacks
		attachVWorldUpdater(sweetDS);
	}

	override   def getUniqueName() : String = {
		"generatedName_99";
	}

	override def getDisplayNode() : JmeNode = {
		val crc  : CogcharRenderContext = getRenderContext();
		val rrc : RenderRegistryClient = crc.getRenderRegistryClient();
		val rootDeepNode = rrc.getJme3RootDeepNode(null)
		rootDeepNode
	}

}

// This is our "app" class in the JME taxonomy.
// It *inherits* a .start() method, and init callback, which eventually calls doMoreSimpleInit.
// It also inherits an enqueue() method that can be used to postpone work into a later task on render thread.
// We use that capability to schedule our real init work.
class SimBalloonJmeApp extends ArbBalloonJmeApp with UpdateAttacher with VWCore {
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

		maybeSendSetupResults()

	}
	private def maybeSendSetupResults(): Unit = {
		if (myResultsTeller_opt.isDefined) {
			val notice = makeSetupResultsNotice()
			getLogger.info("Sending setupResults notice: {}", notice)
			myResultsTeller_opt.get.tellCPMsg(notice)
		} else {
			getLogger().warn("No results teller found, so no setup results are passed back.")
		}

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
		// val bonusTask = makeSampleContentCreationTask
		val notice = new VWSetupResultsNotice(fullIng, fullIng, this, Option(myTMB))
		notice
	}
	/*
	private def makeSampleContentCreationTask : MoreIsolatedBonusContentTask = {
		val crc: CogcharRenderContext = getRenderContext
		// TODO:  Get the flyCam and viewPort from render context/registry stuff, instead of here in the "app".
		val fc : FlyByCamera = getFlyByCamera(); // Defined in CogcharPresumedApp
		val vp : ViewPort = getPrimaryAppViewPort(); // Defined in CogcharPresumedApp
		val miit = new MoreIsolatedBonusContentTask(crc, this, myTMB, fc, vp)
		miit
	}
	*/

	override def destroy : Unit = {
		getLogger.warn("SimBalloonJmeApp.destroy() called, indicating JME app exit.")
		getLogger.error("TODO:  Send PoisonPill to actorSystem, so rest of application can exit.")
		getLogger.info("Now calling super.destroy().")
		super.destroy
	}
}
