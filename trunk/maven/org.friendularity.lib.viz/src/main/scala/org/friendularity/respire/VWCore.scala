package org.friendularity.respire

import akka.actor.{ActorContext, ActorRef}
import com.jme3.math.ColorRGBA
import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.api.space.{GridSpaceFactory, MultiDimGridSpace, CellRangeFactory}
import org.cogchar.api.space.GridSpaceTest._
import org.cogchar.bind.midi.in.{CCParamRouter, TempMidiBridge}
import org.cogchar.render.sys.context.CogcharRenderContext
import org.cogchar.render.sys.registry.RenderRegistryClient
import org.cogchar.render.trial.{TrialCameras, TrialContent, TrialBalloon}


trait VWCore {

}
trait VWSceneGraphMgr extends VWorldJobLogic[VWSceneRq] {
	protected def getRootNode
	override def processMsgUnsafe(msg : VWSceneRq, slf : ActorRef, sndr : ActorRef, actx : ActorContext) : Unit = {
		// msg.processInsideUnsafe(this, slf, sndr, actx)

	}

}
trait SimBalloonLauncher extends VarargsLogging {
	lazy val myTBApp: TrialBalloon = new SimBalloon
	// Generally called on main() thread to do initial app setup.
	def setup: Unit = {
		// Code copied from TrialBalloon.main
		info0("^^^^^^^^^^^^^^^^^^^^^^^^  SimBalloonLauncher.setup() calling initMidi()")
		// Initialize any MIDI stuff.
		myTBApp.initMidi
		info0("^^^^^^^^^^^^^^^^^^^^^^^^  SimBalloonLauncher.setup() calling JME3 start(), which will in turn call TrialBalloon.simpleInitApp()")
		// Start the JME3 Virtual world.
		// We may be blocked in this method until the user confirms Canvas startup.
		myTBApp.start

		// org.cogchar.api.space.GridSpaceTest.goGoGo
		// info0("^^^^^^^^^^^^^^^^^^^^^^^^  SimBalloonLauncher.setup() starting config-load test")
		// app.optLoadConfig();
	//	info0("^^^^^^^^^^^^^^^^^^^^^^^^ SimBalloonLauncher.setup() calling playMidiOutput()")
	//	tbApp.playMidiOutput

		info0("^^^^^^^^^^^^^^^^^^^^^^^^ End of SimBalloonLauncher.setup()")
	}
	// Copied from org.cogchar.api.space.GridSpaceTest
	def gridSpaceTest : Unit = {
		info0("^^^^^^^^^^^^^^^^^^^^^^^^  SimBalloonLauncher starting GridSpace test")


		// This block from x=3,y=-1 to x=5,y=6 extends "beyond" its implied containing cell space, which starts at x=1,y=1
		val cellBlock = CellRangeFactory.makeBlock2D(3, 5, -1, 6)
		info1("BS-GST sez:  CellBlock description={}", cellBlock.describe(1)) // cellFrom == 1 -> base-1 labelling

		val space2D : MultiDimGridSpace = GridSpaceFactory.makeSpace2D(5, 80.0f, 120.0f, 7, -20.0f, 15.0f)
		info1("BS-GST sez:  2D Space description={}", space2D.describe()) // cellFrom == 1 -> base-1 labelling

		val posBlock = space2D.computePosBlockForCellBlock(cellBlock);
		info1("BS-GST sez:  Computed result PosBlock description={}", posBlock.describe)
		val vecOnDiag = posBlock.getVecFromMainDiagonal(2.0f)
		info1("BS-GST sez:  Vec on pos-block diag at 2.0f * MAX ={}", vecOnDiag)

		val vecAtMin = posBlock.getVecFromMainDiagonal(0.0f)
		info1("BS-GST sez:  Vec on pos-block diag at 0.0f * MAX ={}", vecAtMin)

		val blockAt729 = CellRangeFactory.makeUnitBlock3D(7, 2, 9)
		info1("BS-GST sez:  3D unit block at 7,2,9 description={}", blockAt729.describe(1))

		val space3D : MultiDimGridSpace = GridSpaceFactory.makeSpace3D(7, -40.0f, 40.0f, 5, -20.0f, 20.0f, 9, -50.0f, 20.0f);
		info1("BS-GST sez:  3D Space description={}", space2D.describe()) // cellFrom == 1 -> base-1 labelling

		info0("BS-GST sez:  We are now done go()ne.  Goodbye Dear User!")

	}
}
//
class SimBalloon extends BigBalloon {

	// Invoked on JME3 app-init thread, as part of simpleAppInit
	override protected def doMoreSimpleInit: Unit = {
		shving
	}
	def shving: Unit = {
		// Code copied from TrialBalloon.doMoreSimpleInit
		getLogger.info("shving: setting flyCam speed.")
		// Sets the speed of our POV camera movement.  The default is pretty slow.
		flyCam.setMoveSpeed(20)
		val myContent = new TrialContent
		val crc: CogcharRenderContext = getRenderContext
		val rrc: RenderRegistryClient = crc.getRenderRegistryClient
		getLogger.info("shving: will now init, in order: lights, 3D content, 2D content, MIDI controllers, extra cameras")

		myContent.shedLight_onRendThread(crc)
		// The other args besides rrc are superfluous, since they are indirectly accessible through rrc.
		// Note that these other args are all instance variables of this TrialBalloon app, inherited from JME3 SimpleApp.
		myContent.initContent3D_onRendThread(rrc, rootNode)

		viewPort.setBackgroundColor(ColorRGBA.Green)

		// Camera-viewports are placed in the screen coordinate system, so we might consider them to be a kind
		// of 2-D content.  They are part of that layout, anyhoo.
		myContent.initContent2D_onRendThread(rrc, guiNode, assetManager)

		attachVWorldUpdater(myContent)

		val ccpr: CCParamRouter = myTMB.getCCParamRouter
		// Hand the MIDI
		myContent.attachMidiCCs(ccpr)

		val tcam: TrialCameras = new TrialCameras
		tcam.setupCamerasAndViews(rrc, crc, myContent)
		// Hand the MIDI bindings to the camera-aware app.
		// (Disabled until rebuild with this method public)
		tcam.attachMidiCCs(ccpr)
	}
}