package org.friendularity.vwimpl

import akka.actor.Actor
import com.jme3.asset.AssetManager
import com.jme3.input.FlyByCamera
import com.jme3.math.ColorRGBA
import com.jme3.renderer.ViewPort
import com.jme3.scene.{Node => JmeNode}
import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.bind.midi.in.{CCParamRouter, TempMidiBridge}
import org.cogchar.render.sys.context.CogcharRenderContext
import org.cogchar.render.sys.registry.RenderRegistryClient
import org.cogchar.render.trial.{TrialCameras, TrialContent}
import org.friendularity.rbody.DualBodyRecord
import org.friendularity.vwmsg.{VWStageRqMsg, VWBodyRq}


/**
  * Created by StuB22 on 6/21/2016.
  *
  * Manages cameras, viewports and lights
  */
trait VWStageLogic extends VarargsLogging {

	def prepareOpticsStage1(flyCam: FlyByCamera, mainViewPort: ViewPort): Unit = {
		// Sets the speed of our POV camera movement.  The default is pretty slow.
		val moveSpeed : Int = 20
		val bgColor = ColorRGBA.Gray
		info2("prepareOpticsStage1: setting flyCam speed to {}, and background color to {}",
					moveSpeed : Integer, bgColor)
		flyCam.setMoveSpeed(moveSpeed)

		mainViewPort.setBackgroundColor(bgColor)
	}
	def prepareOpticsStage2(crc: CogcharRenderContext, someContent : TrialContent) : TrialCameras = {
		someContent.shedLight_onRendThread(crc)
		val tcam: TrialCameras = new TrialCameras
		val rrc: RenderRegistryClient = crc.getRenderRegistryClient
		tcam.setupCamerasAndViews(rrc, crc, someContent)
		tcam
	}

	def makeOldDummyContent() : TrialContent = {
		val someContent = new TrialContent {
			override def doUpdate(rrc: RenderRegistryClient, tpf: Float): Unit = {
				super.doUpdate(rrc, tpf)
				// flyCam is a controller/wrapper around the cam itself.

			}
		}

		someContent
	}
	def wireDummyContent(someContent : TrialContent, someCameras : TrialCameras,
						 updAtchr: UpdateAttacher, tmb_opt: Option[TempMidiBridge]) : Unit = {
		updAtchr.attachUpdater(someContent) // Adds someContent to list of V-world updaters that get regular ticks
		if (tmb_opt.isDefined) {
			val ccpr: CCParamRouter = tmb_opt.get.getCCParamRouter
			someContent.attachMidiCCs(ccpr)
			// Hand the MIDI bindings to the camera-aware app.
			someCameras.attachMidiCCs(ccpr)
		}

	}

	def setupWithDummyContent(tcont : TrialContent, crc: CogcharRenderContext, updAtchr: UpdateAttacher,
							  tmb_opt: Option[TempMidiBridge]) : Unit = {

		val tcam = prepareOpticsStage2(crc, tcont)

		wireDummyContent(tcont, tcam, updAtchr, tmb_opt)

	}

	def displayDummyContent(dummyCont : TrialContent, rrc: RenderRegistryClient,
							rootDeepNode: JmeNode, guiNode: JmeNode, assetManager: AssetManager) : Unit = {
		// The other args besides rrc are superfluous, since they are indirectly accessible through rrc.
		// Note that these other args are all instance variables of this TrialBalloon app, inherited from JME3 SimpleApp.
		dummyCont.initContent3D_onRendThread(rrc, rootDeepNode)

		// Camera-viewports are placed in the screen coordinate system, so we might consider them to be a kind
		// of 2-D content.  They are part of that layout, anyhoo.
		dummyCont.initContent2D_onRendThread(rrc, guiNode, assetManager)

	}
	def setupMoreContentYay(rrc: RenderRegistryClient, rootDeepNode: JmeNode): Unit = {
		val extra = new ExtraStuff {}
		extra.makeBigGrid(rrc, rootDeepNode)

	}
	def prepareStage(crc: CogcharRenderContext, flyCam: FlyByCamera, rootDeepNode: JmeNode,
					 mainViewPort: ViewPort, guiNode: JmeNode, assetManager: AssetManager,
					 updAtchr: UpdateAttacher, tmb_opt: Option[TempMidiBridge]): Unit = {

		getLogger.info("********************prepareStage begins now.")
		// Code for this method originally copied from cogchar.TrialBalloon.doMoreSimpleInit
		val rrc: RenderRegistryClient = crc.getRenderRegistryClient

		prepareOpticsStage1(flyCam, mainViewPort)
		val tcont = makeOldDummyContent()
		// TODO:  Look for which parts of this need to happen before the "wire" step
		displayDummyContent(tcont, rrc, rootDeepNode, guiNode, assetManager)
		setupWithDummyContent(tcont, crc, updAtchr, tmb_opt)

		setupMoreContentYay(rrc, rootDeepNode)

		getLogger.info("********************prepareStage is done!");
	}
}
class VWStageActor(someThing : AnyRef) extends Actor with VWStageLogic {

	def receive = {
		case vwsrq: VWStageRqMsg => {
			// processBodyRq(vwbrq, self, context)
		}
	}
}