package org.friendularity.vwimpl

import com.jme3.asset.AssetManager
import com.jme3.input.FlyByCamera
import com.jme3.material.Material
import com.jme3.math.ColorRGBA
import com.jme3.renderer.ViewPort
import com.jme3.renderer.queue.RenderQueue
import com.jme3.scene.control.BillboardControl
import com.jme3.scene.{Node => JmeNode}
import org.appdapter.core.name.FreeIdent
import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.bind.midi.in.{CCParamRouter, TempMidiBridge}
import org.cogchar.render.sys.context.CogcharRenderContext
import org.cogchar.render.sys.registry.RenderRegistryClient
import org.cogchar.render.sys.task.Queuer
import org.cogchar.render.trial.{TextBox2D, TrialNexus, TextSpatialFactory, TrialCameras, TrialContent}
import org.friendularity.respire.{Srtw}


/** OBSOLETE - kept only as reminder of old-style "Trial" content+camera+midi init sequence.
  * This stuff contains about 10 steps of testing content + camera setup, all not essential except
  * as standin for better testing with equal/superior client messages.
  *
  * Note also the equivalence of these steps to the Java code in Cogchar's
  * TrialBalloon.doMoreSimpleInit, which we override in our SimBaloonJmeApp.
  */
trait IsolatedBonusContentMaker extends VarargsLogging {

	// This is archived code showing old ordering of our post-Init work.
	// Works for creating some test grid content, an extra camera, and wiring to MIDI controller
	// A newer version of these features is now found in VWStageLogic
	/*
		Copied in Spring 2016 from Cogchar TrialBalloon.doMoreSimpleInit:

			protected def OLD_doItUsingArgs_UNUSED(crc : CogcharRenderContext, flyCam : FlyByCamera, rootDeepNode : JmeNode,
											   mainViewPort : ViewPort, guiNode : JmeNode, assetManager: AssetManager,
											   updAtchr : UpdateAttacher, tmb : TempMidiBridge) : Unit = {


			// Code for this method originally copied from cogchar.TrialBalloon.doMoreSimpleInit
			val rrc: RenderRegistryClient = crc.getRenderRegistryClient
			getLogger.info("IsolatedInitLogic: setting flyCam speed.")
			// Sets the speed of our POV camera movement.  The default is pretty slow.

			flyCam.setMoveSpeed(20)
			val someContent = new TrialContent {
				override def doUpdate (rrc: RenderRegistryClient, tpf: Float)  : Unit = {
					super.doUpdate(rrc, tpf)
					// flyCam is a controller/wrapper around the cam itself.

				}
			}

			getLogger.info("IsolatedInitLogic: will now init, in order: lights, 3D content, bgcolor, 2D content, contentUpdater, MIDI controllers, extra cameras+views")

			someContent.shedLight_onRendThread(crc)
			// The other args besides rrc are superfluous, since they are indirectly accessible through rrc.
			// Note that these other args are all instance variables of this TrialBalloon app, inherited from JME3 SimpleApp.
			someContent.initContent3D_onRendThread(rrc, rootDeepNode)

			mainViewPort.setBackgroundColor(ColorRGBA.Gray)

			// Camera-viewports are placed in the screen coordinate system, so we might consider them to be a kind
			// of 2-D content.  They are part of that layout, anyhoo.
			someContent.initContent2D_onRendThread(rrc, guiNode, assetManager)

			updAtchr.attachUpdater(someContent) // Adds someContent to list of V-world updaters that get regular ticks

			val ccpr: CCParamRouter = tmb.getCCParamRouter

			someContent.attachMidiCCs(ccpr)

			/* If we disable the 3D content above, while keeping TrialCameras, then we get an error
			// in TrialCameras, as it seeks to attach to nodes.
			at org.cogchar.render.app.entity.CameraBinding.attachSceneNodeToCamera(CameraBinding.java:229)
			at org.cogchar.render.trial.TrialCameras.setupCamerasAndViews(TrialCameras.java:79)
			at org.friendularity.respire.SimBalloonJmeApp.shving(VWCore.scala:109)
			*/

			val tcam: TrialCameras = new TrialCameras
			tcam.setupCamerasAndViews(rrc, crc, someContent)
			// Hand the MIDI bindings to the camera-aware app.
			// (Disabled until rebuild with this method public)
			tcam.attachMidiCCs(ccpr)

			val extra = new ExtraStuff {}
			extra.makeBigGridAndAttach_onRendThrd(rrc, rootDeepNode)

			getLogger.info("IsolatedInitLogic is done!");
	}
	*/
}


trait TrialStuffConverted extends VarargsLogging {
	private val letters: String = "abcd\nABCD\nEFGHIKLMNOPQRS\nTUVWXYZ"
	private val digits: String = "1\n234567890"
	private val syms: String = "`~!@#$\n%^&*()-=_+[]\\;',./{}|:<>?"
	private val myCamStatTxt: String = "cam stat\ntext goest\nhere"

	def initContent3D_onRendThread (rrc: RenderRegistryClient, appRootNode: JmeNode) : Unit = {
		val myMainDeepNode = new JmeNode("my_main_deep")
		appRootNode.attachChild(myMainDeepNode)
		val tsf: TextSpatialFactory = new TextSpatialFactory(rrc)
		getLogger.info("Making 3D text spatials for letters, digits, syms = 3 total")
		val myLettersBTS = tsf.makeTextSpatial(letters, 0.2f, RenderQueue.Bucket.Transparent, 6)
		val myDigitsBTS = tsf.makeTextSpatial(digits, 0.1f, RenderQueue.Bucket.Transparent, 6)
		val mySymsBTS = tsf.makeTextSpatial(syms, 0.05f, RenderQueue.Bucket.Transparent, 6)
		myMainDeepNode.attachChild(myLettersBTS)
		myLettersBTS.move(3.0f, 3.0f, -50.0f)
		myMainDeepNode.attachChild(myDigitsBTS)
		myMainDeepNode.attachChild(mySymsBTS)
		myDigitsBTS.move(-10f, -10f, -10f)
		val bbCont: BillboardControl = new BillboardControl
		bbCont.setAlignment(BillboardControl.Alignment.Screen)
		myLettersBTS.addControl(bbCont)
		val tNexus: TrialNexus = new TrialNexus(rrc)
// 		makeRectilinearParamViz(tNexus, rrc)
//		makeDisplayTestCones(rrc)
	}

	def initContent2D_onRendThread (rrc: RenderRegistryClient, parentGUInode: JmeNode, assetMgr: AssetManager) : Unit = {
		val myMainGuiNode = new JmeNode("my_main_gui")
		val tsf: TextSpatialFactory = new TextSpatialFactory(rrc)
		val textWrapPixWidth: Int = 60
		val txtRegScale: Float = 1.0f
		val txtDoubleScale: Float = 2.0f
		parentGUInode.attachChild(myMainGuiNode)
		val guiBucket: RenderQueue.Bucket = RenderQueue.Bucket.Gui
		getLogger.info("Making 2D text spatials for letters, digits, syms = 3 total")
		val  myFlatDigitsBTS = tsf.makeTextSpatial(digits, txtRegScale, guiBucket, textWrapPixWidth)
		val myCamStatBT = tsf.makeTextSpatial(myCamStatTxt, txtRegScale * 0.7f, null, 95)
		val myOverlayEqnBT = tsf.makeTextSpatial("X+Y", txtDoubleScale, null, textWrapPixWidth)
		myFlatDigitsBTS.setLocalTranslation(200.0f, 60.0f, 0.0f)
		myCamStatBT.setLocalTranslation(540.0f, 80.0f, -5.0f)
		myOverlayEqnBT.setLocalTranslation(300.0f, 250.0f, 0.0f)
		myMainGuiNode.attachChild(myCamStatBT)
		myMainGuiNode.attachChild(myFlatDigitsBTS)
		myMainGuiNode.attachChild(myOverlayEqnBT)
		val myFloatingStatBox = new TextBox2D(rrc, new FreeIdent("uri:org.cogchar/goody_inst#camStatBox2D"), "opt init txt", ColorRGBA.White, ColorRGBA.Magenta)
		myFloatingStatBox.setCoordinates(380, 150, -2.5f, 110, 90, Queuer.QueueingStyle.INLINE)
		myFloatingStatBox.setupContentsAndAttachToParent(myMainGuiNode, rrc, assetMgr)
	}
}