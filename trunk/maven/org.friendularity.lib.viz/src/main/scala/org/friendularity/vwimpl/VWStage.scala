package org.friendularity.vwimpl

import akka.actor.Actor
import com.jme3.asset.AssetManager
import com.jme3.input.controls.{Trigger, ActionListener}
import com.jme3.input.{InputManager, FlyByCamera}
import com.jme3.math.ColorRGBA
import com.jme3.renderer.ViewPort
import com.jme3.scene.{Node => JmeNode}
import com.jme3.system.AppSettings

import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.bind.midi.in.{CCParamRouter, TempMidiBridge}
import org.cogchar.platform.gui.keybind.KeyBindingTracker
import org.cogchar.render.sys.context.CogcharRenderContext
import org.cogchar.render.sys.input.VW_InputBindingFuncs
import org.cogchar.render.sys.registry.RenderRegistryClient
import org.cogchar.render.trial.{TrialCameras, TrialContent}
import org.friendularity.rbody.DualBodyRecord
import org.friendularity.vwmsg.{VWorldPublicTellers, VWKeymapBinding_Medial, VWStageOpticsBasic, VWStageEmulateBonusContentAndCams, VWStageRqMsg, VWBodyRq}
import java.util.concurrent.{Callable => ConcurrentCallable, Future}

/**
  * Created by StuB22 on 6/21/2016.
  *
  * Manages cameras, viewports and lights.
  * Also currently manages TrialContent, because it is connected to cameras...
  */

trait VWStageCtx extends VarargsLogging {
	def getCRC : CogcharRenderContext
	def getRRC : RenderRegistryClient
	def getUpdateAttacher : UpdateAttacher
	def getTempMidiBridge_opt : Option[TempMidiBridge]
}
case class StageCtxImpl(crc: CogcharRenderContext, upAtchr : UpdateAttacher, tmb_opt : Option[TempMidiBridge]) extends  VWStageCtx {
	override def getCRC : CogcharRenderContext = crc
	override def getRRC : RenderRegistryClient = getCRC.getRenderRegistryClient
	override def getUpdateAttacher : UpdateAttacher = upAtchr
	override def getTempMidiBridge_opt : Option[TempMidiBridge] = tmb_opt

}
trait EnqHlp {
	def makeJConcurCallable(func: Function0[Unit]) : ConcurrentCallable[Unit] = {
		new ConcurrentCallable[Unit] {
			override def call: Unit = {
				func()
			}
		}
	}
	def enqueueCallable(rrc : RenderRegistryClient, func: Function0[Unit]) : Future[Unit] = {
		val workAppStub = rrc.getWorkaroundAppStub
		val callable = makeJConcurCallable(func)
		workAppStub.enqueue(callable)
	}
}
trait VWStageLogic extends VarargsLogging {

	def prepareIndependentOptics_onRendThrd(flyCam: FlyByCamera, mainViewPort: ViewPort,
											moveSpeed : Int, bgColor: ColorRGBA): Unit = {

		info2("prepareOpticsStage1: setting flyCam speed to {}, and background color to {}",
					moveSpeed : Integer, bgColor)
		// Sets the speed of our POV camera movement.  The default is pretty slow.
		flyCam.setMoveSpeed(moveSpeed)

		mainViewPort.setBackgroundColor(bgColor)
	}
	def prepareCoupledOptics_onRendThrd(crc: CogcharRenderContext, someContent : TrialContent, tcam: TrialCameras) : Unit = {
		someContent.shedLight_onRendThread(crc)
		val rrc: RenderRegistryClient = crc.getRenderRegistryClient
		val crc_orNull_notUsed : CogcharRenderContext = null
		tcam.setupCamerasAndViews(rrc, crc_orNull_notUsed, someContent)
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

	def setupDummyCamsAndWiring(tcont : TrialContent, tcam: TrialCameras, crc: CogcharRenderContext, updAtchr: UpdateAttacher,
								tmb_opt: Option[TempMidiBridge]) : Unit = {

		prepareCoupledOptics_onRendThrd(crc, tcont, tcam)

		wireDummyContentToCamsAndMidi(tcont, tcam, updAtchr, tmb_opt)

	}
	def prepareDummyFeatures_onRendThrd(crc: CogcharRenderContext, parentDeepNode: JmeNode, parentFlatGuiNode: JmeNode,
										assetManager: AssetManager, updAtchr: UpdateAttacher, tmb_opt: Option[TempMidiBridge]): Unit = {

		// Code for this method originally copied from cogchar.TrialBalloon.doMoreSimpleInit
		val rrc: RenderRegistryClient = crc.getRenderRegistryClient

//		prepareOpticsStage1_onRendThrd(flyCam, mainViewPort, moveSpeed, bgColor)
		val tcont = makeOldDummyContent()
		val tcam: TrialCameras = new TrialCameras

		// TODO:  Look for which parts of this need to happen before the "wire" step
		displayDummyContent_onRendThrd(tcont, rrc, parentDeepNode, parentFlatGuiNode, assetManager)
		setupDummyCamsAndWiring(tcont, tcam, crc, updAtchr, tmb_opt)

		getLogger.info("********************prepareStage is done!");
	}
	def sendRendTaskForDummyFeatures(crc : CogcharRenderContext, updAtchr : UpdateAttacher,
									 tmb_opt : Option[TempMidiBridge]) : Unit = {

		val rrc: RenderRegistryClient = crc.getRenderRegistryClient
		val rootDeepNode = rrc.getJme3RootDeepNode(null)
		val rootFlatNode = rrc.getJme3RootOverlayNode(null)
		val assetMgr = rrc.getJme3AssetManager(null)

		val taskForRendThrd  = new ConcurrentCallable[Unit] {
			override def call: Unit = {
				// Note that updAtchr is actually a handle to our JME-App object (SimBalloonJmeApp)
				prepareDummyFeatures_onRendThrd(crc, rootDeepNode, rootFlatNode, assetMgr, updAtchr, tmb_opt)
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
				prepareIndependentOptics_onRendThrd(fbCam, mvp, rq.moveSpeed, rq.bgColor)
			}
		}
		workaroundStub.enqueue(taskForRendThrd)
	}

	private def makeJmeActionListener(vwpt: VWorldPublicTellers,
								   actionsByName : Map[String,Function1[VWorldPublicTellers,Unit]])
			= new ActionListener {
		override def onAction(actionName: String, isPressed: Boolean, tpf: Float): Unit = {
			if (isPressed) {
				val actFunc_opt = actionsByName.get(actionName)
				info2("VWStage Listener for JME:  Resolved actionName={} to func_opt={}", actionName, actFunc_opt)
				if (actFunc_opt.isDefined) {
					actFunc_opt.get.apply(vwpt)
				}
			} else {
				info1("Ignoring un-pressed event at action={}", actionName)
			}
		}
	}
	// Uses additive semantics.  If called multiple times, may result in duplicate or overlapping bindings.
	// To start over, use clearJmeInputMappings... below.
	// All state is kept in the JME mappings + listener bindings.
	def registerKeymap(rrc: RenderRegistryClient, keynamesToActions : Map[String,Function1[VWorldPublicTellers,Unit]],
					   pubTellers : VWorldPublicTellers) : Unit = {
		val jmeInpMgr : InputManager = rrc.getJme3InputManager(null)

		val baloney = new KeyBindingTracker // We don't actually use this, but is wanted by makeJME3InputTriggers

		var theseActionsByName : Map[String,Function1[VWorldPublicTellers,Unit]] = Map()
		for ((keyOrInputName,actFunc) <- keynamesToActions) {
			// 3 steps: 1) create/find a JME trigger, 2) register the trigger to an actionName, and 3) assoc the cback.
			// val actionNameFromChr = "actionFor_" + keyChr
			val ourKeyConstant = VW_InputBindingFuncs.getKeyConstantForName(keyOrInputName)
			if (ourKeyConstant != VW_InputBindingFuncs.NULL_KEY) {
				val actionName = "actFor_" + keyOrInputName
				val triggersArr : Array[Trigger] = VW_InputBindingFuncs.makeJME3InputTriggers(ourKeyConstant, actionName, baloney)
				if (triggersArr.length == 1) {
					info3("Registering triggers={} for action={} at inputCode={}", triggersArr, actionName, ourKeyConstant : Integer)
					jmeInpMgr.addMapping(actionName, triggersArr(0))
				}  else {
					warn3("Got wrong number ot triggers={} for action={} at inputCode={}", triggersArr, actionName, ourKeyConstant : Integer)
				}
				theseActionsByName += (actionName -> actFunc)
			}
		}
		val theseActionNames : Array[String] = theseActionsByName.keys.toArray

		val listener = makeJmeActionListener(pubTellers, theseActionsByName)
		jmeInpMgr.addListener(listener, theseActionNames : _*) // cast to java method varargs

	}

	protected def clearJmeInputMappingsAndRelinkFBCam(rrc: RenderRegistryClient) : Unit = {
		val jmeInpMgr : InputManager = rrc.getJme3InputManager(null)
		jmeInpMgr.clearMappings;
		val workAppStub = rrc.getWorkaroundAppStub
		// Since we just cleared mappings and are (for now at least) using the default FlyByCamera mappings, we must re-register them
		val fbCam: FlyByCamera = workAppStub.getFlyByCamera
		fbCam.registerWithInput(jmeInpMgr)

		// val appSett : AppSettings = rrc.get  - but we have only *set*

	}
}
class VWStageActor(myStageCtx : VWStageCtx) extends Actor with VWStageLogic {

	def receive = {
		case embon : VWStageEmulateBonusContentAndCams => {

			sendRendTaskForDummyFeatures(myStageCtx.getCRC, myStageCtx.getUpdateAttacher, myStageCtx.getTempMidiBridge_opt)

		}
		case opticsBasicRq :	VWStageOpticsBasic => {
			sendRendTaskForOpticsBasic(opticsBasicRq, myStageCtx.getRRC)
		}
		case keymapMedial : VWKeymapBinding_Medial => {
			registerKeymap(myStageCtx.getRRC, keymapMedial.inpNamesToActionFuncs, keymapMedial.pubTellers)
		}
		case vwsrq: VWStageRqMsg => {
			// processBodyRq(vwbrq, self, context)
		}
	}
}