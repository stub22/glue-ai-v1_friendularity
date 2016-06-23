package org.friendularity.vwimpl

import akka.actor.Actor
import com.jme3.asset.AssetManager
import com.jme3.input.FlyByCamera
import com.jme3.math.ColorRGBA
import com.jme3.renderer.ViewPort
import com.jme3.scene.{Node => JmeNode}
import com.sun.javafx.image.impl.ByteBgraPre.ToByteBgraConv
import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.bind.midi.in.{CCParamRouter, TempMidiBridge}
import org.cogchar.render.sys.context.CogcharRenderContext
import org.cogchar.render.sys.registry.RenderRegistryClient
import org.cogchar.render.trial.{TrialCameras, TrialContent}
import org.friendularity.rbody.DualBodyRecord
import org.friendularity.vwmsg.{VWStageOpticsBasic, VWStageEmulateBonusContentAndCams, VWStageRqMsg, VWBodyRq}
import java.util.concurrent.{Callable => ConcurrentCallable}

/**
  * Created by StuB22 on 6/21/2016.
  *
  * Manages cameras, viewports and lights.
  * Also currently manages TrialContent, because it is connected to cameras...
  */
trait VWStageLogic extends VarargsLogging {

	def prepareOpticsStage1_onRendThrd(flyCam: FlyByCamera, mainViewPort: ViewPort,
									   moveSpeed : Int, bgColor: ColorRGBA): Unit = {

		info2("prepareOpticsStage1: setting flyCam speed to {}, and background color to {}",
					moveSpeed : Integer, bgColor)
		// Sets the speed of our POV camera movement.  The default is pretty slow.
		flyCam.setMoveSpeed(moveSpeed)

		mainViewPort.setBackgroundColor(bgColor)
	}
	def prepareOpticsStage2_onRendThrd(crc: CogcharRenderContext, someContent : TrialContent) : TrialCameras = {
		someContent.shedLight_onRendThread(crc)
		val tcam: TrialCameras = new TrialCameras
		val rrc: RenderRegistryClient = crc.getRenderRegistryClient
		val crc_orNull_notUsed : CogcharRenderContext = null
		tcam.setupCamerasAndViews(rrc, crc_orNull_notUsed, someContent)
		tcam
	}

	def makeOldDummyContent() : TrialContent = {
		// Visually this includes the dense purple+green+white grid-forest that Sinbad is standing in,
		// and the overlay 2D text boxes used to display camera positions, also some orangey pointer cones.
		val someContent = new TrialContent {
			override def doUpdate(rrc: RenderRegistryClient, tpf: Float): Unit = {
				super.doUpdate(rrc, tpf)
				// flyCam is a controller/wrapper around the cam itself.

			}
		}

		someContent
	}
	def displayDummyContent_onRendThrd(dummyCont : TrialContent, rrc: RenderRegistryClient,
									   rootDeepNode: JmeNode, guiNode: JmeNode, assetManager: AssetManager) : Unit = {
		// The other args besides rrc are superfluous, since they are indirectly accessible through rrc.
		// Note that these other args are all instance variables of this TrialBalloon app, inherited from JME3 SimpleApp.
		dummyCont.initContent3D_onRendThread(rrc, rootDeepNode)

		// Camera-viewports are placed in the screen coordinate system, so we might consider them to be a kind
		// of 2-D content.  They are part of that layout, anyhoo.
		dummyCont.initContent2D_onRendThread(rrc, guiNode, assetManager)

	}


	def wireDummyContentToCamsAndMidi(someContent : TrialContent, someCameras : TrialCameras,
									  updAtchr: UpdateAttacher, tmb_opt: Option[TempMidiBridge]) : Unit = {
		updAtchr.attachUpdater(someContent) // Adds someContent to list of V-world updaters that get regular ticks
		if (tmb_opt.isDefined) {
			val ccpr: CCParamRouter = tmb_opt.get.getCCParamRouter
			someContent.attachMidiCCs(ccpr)
			// Hand the MIDI bindings to the camera-aware app.
			someCameras.attachMidiCCs(ccpr)
		}

	}

	def setupDummyCamsAndWiring(tcont : TrialContent, crc: CogcharRenderContext, updAtchr: UpdateAttacher,
								tmb_opt: Option[TempMidiBridge]) : Unit = {

		val tcam = prepareOpticsStage2_onRendThrd(crc, tcont)

		wireDummyContentToCamsAndMidi(tcont, tcam, updAtchr, tmb_opt)

	}
/*
	def displayMoreModernContentYay_onRendThrd(rrc: RenderRegistryClient, rootDeepNode: JmeNode): Unit = {
		val extra = new ExtraStuff {}
		extra.makeBigGridAndAttach_onRendThrd(rrc, rootDeepNode)

	}
*/
	def prepareStage_onRendThrd(crc: CogcharRenderContext, parentDeepNode: JmeNode, parentFlatGuiNode: JmeNode,
								assetManager: AssetManager, updAtchr: UpdateAttacher, tmb_opt: Option[TempMidiBridge]): Unit = {


		// Code for this method originally copied from cogchar.TrialBalloon.doMoreSimpleInit
		val rrc: RenderRegistryClient = crc.getRenderRegistryClient
//		prepareOpticsStage1_onRendThrd(flyCam, mainViewPort, moveSpeed, bgColor)
		val tcont = makeOldDummyContent()
		// TODO:  Look for which parts of this need to happen before the "wire" step
		displayDummyContent_onRendThrd(tcont, rrc, parentDeepNode, parentFlatGuiNode, assetManager)
		setupDummyCamsAndWiring(tcont, crc, updAtchr, tmb_opt)

		// displayMoreModernContentYay_onRendThrd(rrc, rootDeepNode)

		getLogger.info("********************prepareStage is done!");
	}
	def sendRendTaskForStagePrepare(crc : CogcharRenderContext, // flyCam : FlyByCamera, viewPort : ViewPort,
									updAtchr : UpdateAttacher, tmb_opt : Option[TempMidiBridge]) : Unit = {
		val rrc: RenderRegistryClient = crc.getRenderRegistryClient
		val rootDeepNode = rrc.getJme3RootDeepNode(null)
		val rootFlatNode = rrc.getJme3RootOverlayNode(null)
		val assetMgr = rrc.getJme3AssetManager(null)

		val taskForRendThrd  = new ConcurrentCallable[Unit] {
			override def call: Unit = {
				prepareStage_onRendThrd(crc, rootDeepNode, rootFlatNode, assetMgr, updAtchr, tmb_opt)
			}
		}
		crc.enqueueCallable(taskForRendThrd)
	}
	def sendRendTaskForOpticsBasic(rq : VWStageOpticsBasic, rrc: RenderRegistryClient) : Unit = {
		val workaroundStub = rrc.getWorkaroundAppStub
		val fbCam = workaroundStub.getFlyByCamera

		val mvp = workaroundStub.getPrimaryAppViewPort
		val taskForRendThrd  = new ConcurrentCallable[Unit] {
			override def call: Unit = {
				prepareOpticsStage1_onRendThrd(fbCam, mvp, rq.moveSpeed, rq.bgColor)
			}
		}
		workaroundStub.enqueue(taskForRendThrd)
	}
}
class VWStageActor(myBonusTask : MoreIsolatedBonusContentTask) extends Actor with VWStageLogic {

	def receive = {
		case embon : VWStageEmulateBonusContentAndCams => {
			val task = myBonusTask //  embon.bonusTask
			sendRendTaskForStagePrepare(task.crc, // task.myFlyCam, task.myMainViewPort,
								task.upAtchr, Option(task.tmb))
		}
		case opticsBasicRq :	VWStageOpticsBasic => {
			sendRendTaskForOpticsBasic(opticsBasicRq, myBonusTask.crc.getRenderRegistryClient)
		}
		case vwsrq: VWStageRqMsg => {
			// processBodyRq(vwbrq, self, context)
		}
	}
}