package org.friendularity.respire

import akka.actor.{ActorContext, ActorRef}
import com.jme3.asset.AssetManager
import com.jme3.input.FlyByCamera
import com.jme3.math.ColorRGBA
import com.jme3.renderer.ViewPort
import com.jme3.scene.{Node => JmeNode}
import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.api.space.{GridSpaceFactory, MultiDimGridSpace, CellRangeFactory}
import org.cogchar.api.space.GridSpaceTest._
import org.cogchar.bind.midi.in.{CCParamRouter, TempMidiBridge}
import org.cogchar.render.sys.context.CogcharRenderContext
import org.cogchar.render.sys.registry.RenderRegistryClient
import org.cogchar.render.trial.{TrialUpdater, TrialCameras, TrialContent, TrialBalloon}


trait VWCore {

}
trait VWSceneGraphMgr extends VWorldJobLogic[VWSceneRq] {
	protected def getRootNode
	override def processMsgUnsafe(msg : VWSceneRq, slf : ActorRef, sndr : ActorRef, actx : ActorContext) : Unit = {
		// msg.processInsideUnsafe(this, slf, sndr, actx)

	}

}
trait SimBalloonAppLauncher extends VarargsLogging {
	lazy val mySBApp: SimBalloonJmeApp = new SimBalloonJmeApp
	// Generally called on main() thread to do initial app setup.
	def setup: Unit = {
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
class SimBalloonJmeApp extends BigBalloonJmeApp with UpdateAttacher {

	override def attachUpdater(tu : TrialUpdater) : Unit = {
		attachVWorldUpdater(tu)
	}
	// Invoked on JME3 app-init thread, as part of simpleAppInit, initiated by mySBApp.start above.
	override protected def doMoreSimpleInit: Unit = {
		val initTask = makeInitTask
		// Defer all the interesting work onto a future callback from render-thread
		val initTskCallable = new java.util.concurrent.Callable[Unit] {
			@throws(classOf[Exception])
			override def call : Unit = {
				initTask.doItMostEasily
			}
		}
		val futureWhichWeIgnore = enqueue(initTskCallable)
		// shving
	}
	def makeInitTask : MoreIsolatedInitTask = {
		val crc: CogcharRenderContext = getRenderContext
		// TODO:  Get the flyCam and viewPort from render context/registry stuff, instead of here in the "app".
		val fc : FlyByCamera = flyCam
		val vp : ViewPort = viewPort
		val miit = new MoreIsolatedInitTask(crc, this, myTMB, fc, vp)
		miit
	}

	override def destroy : Unit = {
		getLogger.warn("SimBalloonJmeApp.destroy() called, indicating JME app exit.")
		getLogger.error("TODO:  Send PoisonPill to actorSystem, so rest of application can exit.")
		getLogger.info("Now calling super.destroy().")
		super.destroy
	}
}
trait IsolatedInitLogic extends VarargsLogging {
	protected def doItUsingArgs(crc : CogcharRenderContext, flyCam : FlyByCamera, rootDeepNode : JmeNode,
								mainViewPort : ViewPort, guiNode : JmeNode, assetManager: AssetManager,
								updAtchr : UpdateAttacher, tmb : TempMidiBridge) : Unit = {
		// Code for this method originally copied from cogchar.TrialBalloon.doMoreSimpleInit
		val rrc: RenderRegistryClient = crc.getRenderRegistryClient
		getLogger.info("shving: setting flyCam speed.")
		// Sets the speed of our POV camera movement.  The default is pretty slow.

		flyCam.setMoveSpeed(20)
		val someContent = new TrialContent

		getLogger.info("shving: will now init, in order: lights, 3D content, bgcolor, 2D content, contentUpdater, MIDI controllers, extra cameras+views")

		someContent.shedLight_onRendThread(crc)
		// The other args besides rrc are superfluous, since they are indirectly accessible through rrc.
		// Note that these other args are all instance variables of this TrialBalloon app, inherited from JME3 SimpleApp.
		someContent.initContent3D_onRendThread(rrc, rootDeepNode)

		mainViewPort.setBackgroundColor(ColorRGBA.Green)

		// Camera-viewports are placed in the screen coordinate system, so we might consider them to be a kind
		// of 2-D content.  They are part of that layout, anyhoo.
		someContent.initContent2D_onRendThread(rrc, guiNode, assetManager)

		updAtchr.attachUpdater(someContent) // Adds someContent to list of updaters

		val ccpr: CCParamRouter = tmb.getCCParamRouter

		someContent.attachMidiCCs(ccpr)

		/* If we disable the 3D content, then we get an error in TrialCameras, as it seeks to attach to nodes.
		at org.cogchar.render.app.entity.CameraBinding.attachSceneNodeToCamera(CameraBinding.java:229)
		at org.cogchar.render.trial.TrialCameras.setupCamerasAndViews(TrialCameras.java:79)
		at org.friendularity.respire.SimBalloonJmeApp.shving(VWCore.scala:109)
		*/

		val tcam: TrialCameras = new TrialCameras
		tcam.setupCamerasAndViews(rrc, crc, someContent)
		// Hand the MIDI bindings to the camera-aware app.
		// (Disabled until rebuild with this method public)
		tcam.attachMidiCCs(ccpr)

	}
	protected def doItEasier(crc : CogcharRenderContext, flyCam : FlyByCamera, viewPort : ViewPort, updAtchr : UpdateAttacher, tmb : TempMidiBridge) : Unit = {
		val rrc: RenderRegistryClient = crc.getRenderRegistryClient
		val rootDeepNode = rrc.getJme3RootDeepNode(null)
		val rootFlatNode = rrc.getJme3RootOverlayNode(null)
		val assetMgr = rrc.getJme3AssetManager(null)

		doItUsingArgs(crc, flyCam, rootDeepNode, viewPort, rootFlatNode, assetMgr, updAtchr, tmb)
	}
}

// Shopping list of 7 appy things we need, besides CRC:
// flyCam, rootNode, viewPort, guiNode, assetManager, attachVWorldUpdater(=myUpdaters), myTMB
class IsolatedInitTask(myCRC: CogcharRenderContext, myUpAtchr : UpdateAttacher, myTMB : TempMidiBridge)
			extends IsolatedInitLogic {

	def doItEvenMoreEasily(flyCam : FlyByCamera, viewPort : ViewPort) : Unit = {

		// Can get flyCam from opticFacade?  Appears it currently only keeps track of "default" camera,
		// which is different how?
		// What about viewPort?
		doItEasier(myCRC, flyCam, viewPort, myUpAtchr, myTMB)
	}
}
class MoreIsolatedInitTask(crc: CogcharRenderContext, upAtchr : UpdateAttacher, tmb : TempMidiBridge,
						   myFlyCam : FlyByCamera, myMainViewPort : ViewPort)
			extends IsolatedInitTask(crc, upAtchr, tmb) {

	def doItMostEasily : Unit = {
		doItEvenMoreEasily(myFlyCam, myMainViewPort)
	}
}