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
package org.friendularity.vwimpl

import akka.actor.Actor
import com.jme3.asset.AssetManager
import com.jme3.input.controls.{Trigger, ActionListener}
import com.jme3.input.{InputManager, FlyByCamera}
import com.jme3.math.{Rectangle, Vector2f, Vector3f, ColorRGBA}
import com.jme3.renderer.{Camera, ViewPort}
import com.jme3.scene.control.CameraControl
import com.jme3.scene.{Node => JmeNode, CameraNode}
import com.jme3.system.AppSettings
import org.appdapter.core.name.Ident

import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.bind.midi.in.{CCParamRouter, TempMidiBridge}
import org.cogchar.platform.gui.keybind.KeyBindingTracker
import org.cogchar.render.app.core.WorkaroundAppStub;
import org.cogchar.render.app.entity.CameraBinding
import org.cogchar.render.opengl.optic.CameraMgr
import org.cogchar.render.sys.context.CogcharRenderContext
import org.cogchar.render.sys.input.VW_InputBindingFuncs
import org.cogchar.render.sys.registry.RenderRegistryClient
import org.cogchar.render.sys.task.Queuer
import org.cogchar.render.trial.{PointerCone, TrialCameras, TrialContent}
import org.friendularity.field.{MsgToStatusSrc, ReportSourceCtrlMsg, ReportingTickChance, MediumFieldDataBag, ItemFieldSpecDirectImpl, VWTestFieldIdents, ReportFilteringPolicy, ItemFieldData, MonitoredSpaceImpl}

import org.friendularity.rbody.DualBodyRecord
import org.friendularity.vwmsg.{VWSetupOvlBookRq, NavCmd, InnerNavCmds, VWSCR_ExistingNode, CamState3D, ViewportDesc, VWModifyCamStateRq, VWCreateCamAndViewportRq, VWBindCamNodeRq, VWorldPublicTellers, VWKeymapBinding_Medial, VWStageOpticsBasic, VWStageEmulateBonusContentAndCams, VWStageRqMsg, VWBodyRq}

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
case class StageCtxImpl(crc: CogcharRenderContext, upAtchr : UpdateAttacher, tmb_opt : Option[TempMidiBridge])
			extends  VWStageCtx {

	override def getCRC : CogcharRenderContext = crc
	override def getRRC : RenderRegistryClient = getCRC.getRenderRegistryClient
	override def getUpdateAttacher : UpdateAttacher = upAtchr
	override def getTempMidiBridge_opt : Option[TempMidiBridge] = tmb_opt

}
trait DummyContentLogic extends VarargsLogging with EnqHlp {
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
	def prepareCoupledOptics_onRendThrd(crc: CogcharRenderContext, someContent : TrialContent, tcam: TrialCameras) : Unit = {

		val rrc: RenderRegistryClient = crc.getRenderRegistryClient
		val crc_orNull_notUsed : CogcharRenderContext = null
		tcam.setupCamerasAndViews(rrc, crc_orNull_notUsed, someContent)
	}

	def setupDummyCamsAndWiring(tcont : TrialContent, tcam: TrialCameras, crc: CogcharRenderContext, updAtchr: UpdateAttacher,
								tmb_opt: Option[TempMidiBridge]) : Unit = {

		prepareCoupledOptics_onRendThrd(crc, tcont, tcam)

		wireDummyContentToCamsAndMidi(tcont, tcam, updAtchr, tmb_opt)

	}
	def prepareDummyFeatures_onRendThrd(crc: CogcharRenderContext, parentDeepNode: JmeNode, parentFlatGuiNode: JmeNode,
										assetManager: AssetManager, updAtchr: UpdateAttacher, tmb_opt: Option[TempMidiBridge]): Unit = {
		getLogger.info("************** prepareDummyFeatures_onRendThrd - START")
		// Code for this method originally copied from cogchar.TrialBalloon.doMoreSimpleInit
		val rrc: RenderRegistryClient = crc.getRenderRegistryClient

		val tcont = makeOldDummyContent()

		tcont.shedLight_onRendThread(crc)

		// Disabled 2016-08-11	 val tcam: TrialCameras = new TrialCameras

		// Disabled 2016-08-11   displayDummyContent_onRendThrd(tcont, rrc, parentDeepNode, parentFlatGuiNode, assetManager)

		// Disabled 2016-08-11   setupDummyCamsAndWiring(tcont, tcam, crc, updAtchr, tmb_opt)

		getLogger.info("********************prepareDummyFeatures_onRendThrd - END");
	}
	def sendRendTaskForDummyFeatures(crc : CogcharRenderContext, updAtchr : UpdateAttacher,
									 tmb_opt : Option[TempMidiBridge]) : Unit = {

		val rrc: RenderRegistryClient = crc.getRenderRegistryClient
		val rootDeepNode = rrc.getJme3RootDeepNode(null)
		val rootFlatNode = rrc.getJme3RootOverlayNode(null)
		val assetMgr = rrc.getJme3AssetManager(null)

		val senderCallable : Function0[Unit] = () => {
			// Note that updAtchr is actually a handle to our JME-App object (SimBalloonJmeApp)
			prepareDummyFeatures_onRendThrd(crc, rootDeepNode, rootFlatNode, assetMgr, updAtchr, tmb_opt)
		}
		enqueueJmeCallable(rrc, senderCallable)
	}

}
trait BasicOpticsLogic extends VarargsLogging with EnqHlp {
	def prepareIndependentOptics_onRendThrd(workaroundStub: WorkaroundAppStub, flyCam: FlyByCamera, mainViewPort: ViewPort,
											moveSpeed : Int, bgColor: ColorRGBA): Unit = {

		info2("prepareOpticsStage1: setting flyCam speed to {}, and background color to {}",
			moveSpeed : Integer, bgColor)
		// Sets the speed of our POV camera movement.  The default is pretty slow.
		flyCam.setMoveSpeed(moveSpeed)
        
		mainViewPort.setBackgroundColor(bgColor)
        workaroundStub.setPauseOnLostFocus(false)
	}
	def sendRendTaskForOpticsBasic(rq : VWStageOpticsBasic, rrc: RenderRegistryClient) : Unit = {
		val workaroundStub = rrc.getWorkaroundAppStub
		val fbCam = workaroundStub.getFlyByCamera
        
		val mvp = workaroundStub.getPrimaryAppViewPort
		val senderCallable : Function0[Unit] = () => {
			prepareIndependentOptics_onRendThrd(workaroundStub, fbCam, mvp, rq.moveSpeed, rq.bgColor)
		}
		enqueueJmeCallable(rrc, senderCallable)
	}
}
trait VWStageLogic extends DummyContentLogic with BasicOpticsLogic with VarargsLogging with EnqHlp {


}

class VWStageActor(myStageCtx : VWStageCtx) extends Actor with VWStageLogic with VWCamLogic
			with VWKeyMapLogic with OverlayLogic {

	override def getStageCtx : VWStageCtx = myStageCtx

	// private var myOvlBook_opt : Option[OverlayLogic] = None

	override def receive : Actor.Receive = {
		case embon : VWStageEmulateBonusContentAndCams => {

			sendRendTaskForDummyFeatures(myStageCtx.getCRC, myStageCtx.getUpdateAttacher, myStageCtx.getTempMidiBridge_opt)

		}
		case opticsBasicRq :	VWStageOpticsBasic => {
			sendRendTaskForOpticsBasic(opticsBasicRq, myStageCtx.getRRC)
		}
		case keymapMedial : VWKeymapBinding_Medial => {
			registerKeymap(myStageCtx.getRRC, keymapMedial.inpNamesToActionFuncs, keymapMedial.pubTellers)
		}

		case makeCam : VWCreateCamAndViewportRq => {
			// TODO:  We *think* this call needs to be deferred onto rend-thrd
			makeCam_rendThrd(makeCam)
		}
		case ucs : VWModifyCamStateRq => {
			// TODO:  We *think* this call needs to be deferred onto rend-thrd
			updateCamState_rendThrd(ucs)
		}
		case bindCamNode : VWBindCamNodeRq => {
			processBindCamNode(bindCamNode)
		}
		case msgToStatusSrc: MsgToStatusSrc => {
			processMsgToStatusSrc(msgToStatusSrc)
		}
		case otherStageRq: VWStageRqMsg => {
			// processBodyRq(vwbrq, self, context)
		}
	}
}