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
package org.friendularity.vw.impl.stg

import java.util.concurrent.{Callable => ConcurrentCallable}

import akka.actor.Actor
import com.jme3.asset.AssetManager
import com.jme3.input.FlyByCamera
import com.jme3.light.DirectionalLight
import com.jme3.math.{ColorRGBA, Vector3f}
import com.jme3.renderer.{Camera, ViewPort}
import com.jme3.scene.{Node => JmeNode, Spatial}
import com.jme3.texture.Texture
import com.jme3.util.SkyFactory
import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.bind.midi.in.{CCParamRouter, TempMidiBridge}
import org.cogchar.render.app.core.WorkaroundAppStub
import org.cogchar.render.opengl.optic.LightFactory
import org.cogchar.render.sys.context.CogcharRenderContext
import org.cogchar.render.sys.registry.RenderRegistryClient
import org.cogchar.render.trial.{TrialCameras, TrialContent}
import org.friendularity.infra.field.MsgToStatusSrc
import org.friendularity.vw.impl.cam.VWCamLogic
import org.friendularity.vw.impl.inp.VWKeyMapLogic
import org.friendularity.vw.impl.ovl.OverlayLogic
import org.friendularity.vw.impl.sys.UpdateAttacher
import org.friendularity.vw.impl.tsk.JmeEnqHlp
import org.friendularity.vw.msg.stg.{VWBindCamNodeRq, VWCreateCamAndViewportRq, VWKeymapBinding_Medial, VWModifyCamStateRq, VWStageBackgroundColor, VWStageBackgroundSkybox, VWStageEmulateBonusContentAndCams, VWStageOpticsBasic, VWStageRqMsg, VWStageSetupLighting, VWStatsViewMessage}

/**
  * Created by StuB22 on 6/21/2016.
  *
  * Manages cameras, viewports and lights.
  * Also currently manages TrialContent, because it is connected to cameras...
  */


trait DummyContentLogic extends VarargsLogging with JmeEnqHlp {
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
trait BasicOpticsLogic extends VarargsLogging with JmeEnqHlp {
	
	var skyBox_opt : Option[Spatial] = Option.empty[Spatial]
	var backgroundColor_opt : Option[ColorRGBA] = Option.empty[ColorRGBA]
	
	
	def prepareIndependentOptics_onRendThrd(workaroundStub: WorkaroundAppStub, flyCam: FlyByCamera, mainViewPort: ViewPort,
											moveSpeed : Int, pauseOnLostFocus: Boolean, dragMouseToRotateCamera : Boolean): Unit = {

		info1("prepareOpticsStage1: setting flyCam speed to {}", moveSpeed : Integer)
		// Sets the speed of our POV camera movement.  The default is pretty slow.
		flyCam.setMoveSpeed(moveSpeed)
		flyCam.setDragToRotate(dragMouseToRotateCamera)
        
        workaroundStub.setPauseOnLostFocus(pauseOnLostFocus)
	}
    
	def setBackgroundColor_onRendThrd(mainViewPort: ViewPort, parentDeepNode: JmeNode,  bgColor: ColorRGBA): Unit = {

		info1("prepareOpticsStage1: setting background color to {}", bgColor)
		if (skyBox_opt.isDefined){
			removeSkyBox_onRendThread(parentDeepNode)
		}
		
		mainViewPort.setBackgroundColor(bgColor)
	}
	
	def  removeSkyBox_onRendThread(parentDeepNode: JmeNode) : Unit = {
		info0("SkyBox is already set. Removing SkyBox.")
		parentDeepNode.detachChild(skyBox_opt.get)
		skyBox_opt = Option.empty[Spatial]
	}
	
	def mayLoadTexture_OnRenderThread(assetManager: AssetManager, imagePath: String) : Option[Texture] = {
		var texture_opt : Option[Texture] = Option.empty[Texture]
		
		try{
			/*
			 * The JME documentation says: assetManager.loadTexture returns The loaded texture or 
			 * null if failed to be loaded.
			 * 
			 * But in practice an AssetNotFoundException is thrown.
			 */
			texture_opt  = Option(assetManager.loadTexture(imagePath))
			if(texture_opt.isEmpty){
				error1("Failed to load texture from: {}", imagePath)
			}
		
			trace1("Successfully loaded texture from: {}", texture_opt)
		} catch{
        case e: com.jme3.asset.AssetNotFoundException => error1("Failed to load texture from: {}", imagePath)
      }

		texture_opt
	}
	
	 /**
     * This will load a skybox background instead of a static color background. Asset is found in the
     * content project, textures/skybox/
     *
	 *
	 *
     * Ex. textures/skybox/Default/West.png
     */
    def setBackgroundSkyBox_onRendThrd(parentDeepNode: JmeNode, assetManager: AssetManager, 
				  northImagePath: String, eastImagePath: String, southImagePath: String,
				  westImagePath: String, upImagePath: String, downImagePath: String) : Unit = {
		info3("Setting background skybox with the following images. [North: {}, East: {}, South: {}]",
		northImagePath, eastImagePath, southImagePath)
		info3("Setting background skybox with the following images. [West: {}, Up: {}, Down: {}]",
		 westImagePath, upImagePath, downImagePath)
		
		if (skyBox_opt.isDefined){
			removeSkyBox_onRendThread(parentDeepNode)
		}
		
		val northTexture : Option[Texture] = mayLoadTexture_OnRenderThread(assetManager, northImagePath)
		val eastTexture : Option[Texture] = mayLoadTexture_OnRenderThread(assetManager, eastImagePath)
		val southTexture : Option[Texture] = mayLoadTexture_OnRenderThread(assetManager, southImagePath)
		val westTexture : Option[Texture] = mayLoadTexture_OnRenderThread(assetManager, westImagePath)
		val upTexture : Option[Texture] = mayLoadTexture_OnRenderThread(assetManager, upImagePath)
		val downTexture : Option[Texture] = mayLoadTexture_OnRenderThread(assetManager, downImagePath)
		
		if(northTexture.isEmpty || eastTexture.isEmpty || southTexture.isEmpty 
		|| westTexture.isEmpty || upTexture.isEmpty || downTexture.isEmpty){
			error0("Tried to set SkyBox. Could not load all of the SkyBox's textures. Ensure paths to SkyBox texture images are correct.")
			return
		}
			
		skyBox_opt = Some(SkyFactory.createSky(assetManager, westTexture.get, eastTexture.get,
				northTexture.get, southTexture.get, upTexture.get, downTexture.get))
		parentDeepNode.attachChild(skyBox_opt.get);
    }
   
  
	def sendRendTaskForOpticsBasic(rq : VWStageOpticsBasic, rrc: RenderRegistryClient) : Unit = {
		val workaroundStub = rrc.getWorkaroundAppStub
		val fbCam = workaroundStub.getFlyByCamera
		val mvp = workaroundStub.getPrimaryAppViewPort
		val senderCallable : Function0[Unit] = () => {
			prepareIndependentOptics_onRendThrd(workaroundStub, fbCam, mvp, rq.moveSpeed, rq.pauseOnLostFocus, rq.dragMouseToRotateCamera)
		}
		enqueueJmeCallable(rrc, senderCallable)
	}
	
	def sendRendTaskForBackgroundSkyBox(rq : VWStageBackgroundSkybox, rrc: RenderRegistryClient) : Unit = {
		val workaroundStub = rrc.getWorkaroundAppStub
		val mvp = workaroundStub.getPrimaryAppViewPort
		val parentDeepNode: JmeNode = rrc.getJme3RootDeepNode(null)
		val assetManager: AssetManager = rrc.getJme3AssetManager(null)
		val senderCallable : Function0[Unit] = () => {
			setBackgroundSkyBox_onRendThrd(parentDeepNode, assetManager, rq.northImagePath, rq.eastImagePath, rq.southImagePath,
																		 rq.westImagePath, rq.upImagePath, rq.downImagePath)
		}
		enqueueJmeCallable(rrc, senderCallable)
	}
	
	def sendRendTaskForBackgroundColor(rq : VWStageBackgroundColor, rrc: RenderRegistryClient) : Unit = {
		val workaroundStub = rrc.getWorkaroundAppStub
		val mvp = workaroundStub.getPrimaryAppViewPort
		val parentDeepNode: JmeNode = rrc.getJme3RootDeepNode(null)
		val senderCallable : Function0[Unit] = () => {
			setBackgroundColor_onRendThrd(mvp, parentDeepNode, rq.bgColor)
		}
		enqueueJmeCallable(rrc, senderCallable)
	}
}
trait StatsViewLogic extends VarargsLogging with JmeEnqHlp {
	def setStatsView_onRendThrd(simpleApplication : com.jme3.app.SimpleApplication, displayContentStatsOnScreen: Boolean, displayFPSOnScreen: Boolean): Unit = {
		info2("setting stats view: setting displayContentStatsOnScreen to {} and displayFPSOnScreen to {}",
              displayContentStatsOnScreen.toString, displayFPSOnScreen.toString)
        
        simpleApplication.setDisplayFps(displayFPSOnScreen)
        simpleApplication.setDisplayStatView(displayContentStatsOnScreen)
	}
	def sendRendTaskForStatsView(statsViewMessage : VWStatsViewMessage, registryClient: RenderRegistryClient) : Unit = {
        val simpleApplication : com.jme3.app.SimpleApplication = registryClient.getJme3AppStateManager(null).getApplication.asInstanceOf[com.jme3.app.SimpleApplication]
    
		val senderCallable : Function0[Unit] = () => {
			setStatsView_onRendThrd(simpleApplication, statsViewMessage.displayContentStatsOnScreen, statsViewMessage.displayFPSOnScreen)
		}
		enqueueJmeCallable(registryClient, senderCallable)
	}
}
trait LightingSetupLogic extends VarargsLogging with JmeEnqHlp {
	
	/**
	 * The 2014 virtual world had ambient lighting where {@code 
	 * ambientLightingColor = new ColorRGBA(0.8f, 0.8f, 0.8f, 1f)}
	 */
	def addAmbientLighting_onRenderThread(rootNode : com.jme3.scene.Node, 
										  ambientLightingColor : ColorRGBA): Unit = {
		
		info1("setting ambient light: color: {}", ambientLightingColor.toString)
		LightFactory.addAmbientLight(rootNode, ambientLightingColor)
	}
	
	/**
	 * The 2014 virtual world had a directional light where {@code 
	 * lightingDirection = new Vector3f(-0.1f, -0.7f, -1f)} 
	 * and {@code lightingColor = new ColorRGBA(1f, 1f, 1f, 1f)}
	 */
	def addDirectionalLighting_onRenderThread(rootNode : com.jme3.scene.Node, 
											  lightingDirection : Vector3f,
											  lightingColor : ColorRGBA, 
											  cogcharRenderContext: CogcharRenderContext): Unit = {
		
		info2("setting directional light: direction to {} and color to {}", 
						lightingDirection.toString, lightingColor.toString)
		
		val factory : LightFactory = new LightFactory()
		factory.setParentNode(rootNode)
		val directionalLight : DirectionalLight = factory.makeDirectionalLight(lightingDirection, lightingColor)
		factory.addLightOnMainThread(directionalLight,  cogcharRenderContext)
	}
	
	
	def sendRendTaskForLightingSetup(rq : VWStageSetupLighting, registryClient: RenderRegistryClient, 
									 cogcharRenderContext: CogcharRenderContext) : Unit = {
		val rootNode : com.jme3.scene.Node = registryClient.getJme3RootDeepNode(null)
		
		val senderCallable : Function0[Unit] = () => {
			addAmbientLighting_onRenderThread(rootNode, rq.ambientLightColor)
		}
		enqueueJmeCallable(registryClient, senderCallable)
	}
}
trait VWStageLogic extends DummyContentLogic with BasicOpticsLogic with StatsViewLogic with LightingSetupLogic with VarargsLogging with JmeEnqHlp {


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
		case backroundColorRq :	VWStageBackgroundColor => {
			sendRendTaskForBackgroundColor(backroundColorRq, myStageCtx.getRRC)
		}
		case backgroundSkyBoxRq :	VWStageBackgroundSkybox => {
			sendRendTaskForBackgroundSkyBox(backgroundSkyBoxRq, myStageCtx.getRRC)
		}
        case statsViewRq :	VWStatsViewMessage => {
			sendRendTaskForStatsView(statsViewRq, myStageCtx.getRRC)
		}
        case setupLightingRq :	VWStageSetupLighting => {
			sendRendTaskForLightingSetup(setupLightingRq, myStageCtx.getRRC, myStageCtx.getCRC)
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