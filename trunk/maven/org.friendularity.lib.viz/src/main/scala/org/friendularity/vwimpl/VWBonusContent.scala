package org.friendularity.vwimpl

import com.jme3.asset.AssetManager
import com.jme3.input.FlyByCamera
import com.jme3.material.Material
import com.jme3.math.ColorRGBA
import com.jme3.renderer.ViewPort
import com.jme3.scene.{Node => JmeNode}
import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.bind.midi.in.{CCParamRouter, TempMidiBridge}
import org.cogchar.render.sys.context.CogcharRenderContext
import org.cogchar.render.sys.registry.RenderRegistryClient
import org.cogchar.render.trial.{TrialCameras, TrialContent}
import org.friendularity.respire.{Srtw}


/** OBSOLETE - kept only as reminder of old-style "Trial" content+camera+midi init sequence.
  * This stuff contains about 10 steps of testing content + camera setup, all not essential except
  * as standin for better testing with equal/superior client messages.
  */
trait IsolatedBonusContentMaker extends VarargsLogging {

	// This is archived code showing old ordering of our post-Init work.
	// Works for creating some test grid content, an extra camera, and wiring to MIDI controller
	// A newer version of these features is now found in VWStageLogic
	/*
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
