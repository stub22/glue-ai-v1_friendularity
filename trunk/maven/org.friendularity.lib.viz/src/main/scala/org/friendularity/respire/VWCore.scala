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
class SimBalloonJmeApp extends BigBalloon with UpdateAttacher {

	override def attachUpdater(tu : TrialUpdater) : Unit = {
		attachVWorldUpdater(tu)
	}
	// Invoked on JME3 app-init thread, as part of simpleAppInit, initiated by mySBApp.start above.
	override protected def doMoreSimpleInit: Unit = {
		// Defer all the interesting work onto a future callback from render-thread
		val shvingClbl = new java.util.concurrent.Callable[Unit] {
			@throws(classOf[Exception])
			override def call : Unit = {
				shving
			}
		}
		val futureWhichWeIgnore = enqueue(shvingClbl)
		// shving
	}
	// Here is our delayed init callback.
	def shving: Unit = {
		// Code for this method originally copied from cogchar.TrialBalloon.doMoreSimpleInit
		val crc: CogcharRenderContext = getRenderContext
		val isoDI = new IsolatedDelayedInitTask(crc)
		val fc : FlyByCamera = flyCam
		val rn : JmeNode = rootNode
		val vp : ViewPort = viewPort
		val gn : JmeNode = guiNode
		val am : AssetManager = assetManager
		isoDI.doIt(fc, rn, vp, gn, am, this, myTMB)
	}
	override def destroy : Unit = {
		getLogger.warn("SimBalloonJmeApp.destroy() called, indicating JME app exit.")
		getLogger.error("TODO:  Send PoisonPill to actorSystem, so rest of application can exit.")
		getLogger.info("Now calling super.destroy().")
		super.destroy
	}
}
// Shopping list of 7 appy things we need, besides CRC:
// flyCam, rootNode, viewPort, guiNode, assetManager, attachVWorldUpdater(=myUpdaters), myTMB
class IsolatedDelayedInitTask(myCRC: CogcharRenderContext) extends VarargsLogging {
	def doIt(flyCam : FlyByCamera, rootNode : JmeNode, viewPort : ViewPort, guiNode : JmeNode,
			 		assetManager: AssetManager, updAtchr : UpdateAttacher, tmb : TempMidiBridge) : Unit = {
		val rrc: RenderRegistryClient = myCRC.getRenderRegistryClient
		getLogger.info("shving: setting flyCam speed.")
		// Sets the speed of our POV camera movement.  The default is pretty slow.
		// TODO:  Get this flyCam, viewPort, etc. from render context/registry
		flyCam.setMoveSpeed(20)
		val myContent = new TrialContent

		getLogger.info("shving: will now init, in order: lights, 3D content, 2D content, MIDI controllers, extra cameras")

		myContent.shedLight_onRendThread(myCRC)
		// The other args besides rrc are superfluous, since they are indirectly accessible through rrc.
		// Note that these other args are all instance variables of this TrialBalloon app, inherited from JME3 SimpleApp.
		myContent.initContent3D_onRendThread(rrc, rootNode)

		viewPort.setBackgroundColor(ColorRGBA.Green)

		// Camera-viewports are placed in the screen coordinate system, so we might consider them to be a kind
		// of 2-D content.  They are part of that layout, anyhoo.
		myContent.initContent2D_onRendThread(rrc, guiNode, assetManager)

		updAtchr.attachUpdater(myContent) // Adds myContent to list of updaters

		val ccpr: CCParamRouter = tmb.getCCParamRouter

		myContent.attachMidiCCs(ccpr)

		/* If we disable the 3D content, then we get an error in TrialCameras.
		at org.cogchar.render.app.entity.CameraBinding.attachSceneNodeToCamera(CameraBinding.java:229)
		at org.cogchar.render.trial.TrialCameras.setupCamerasAndViews(TrialCameras.java:79)
		at org.friendularity.respire.SimBalloonJmeApp.shving(VWCore.scala:109)
		*/

		val tcam: TrialCameras = new TrialCameras
		tcam.setupCamerasAndViews(rrc, myCRC, myContent)
		// Hand the MIDI bindings to the camera-aware app.
		// (Disabled until rebuild with this method public)
		tcam.attachMidiCCs(ccpr)

	}
}
