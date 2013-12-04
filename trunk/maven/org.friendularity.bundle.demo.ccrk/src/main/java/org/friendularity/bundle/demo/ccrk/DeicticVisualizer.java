/*
 *  Copyright 2013 by The Friendularity Project (www.friendularity.org).
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

package org.friendularity.bundle.demo.ccrk;

import org.appdapter.core.log.BasicDebugger;
import org.appdapter.core.name.FreeIdent;
import org.appdapter.core.name.Ident;

import org.cogchar.api.cinema.CameraConfig;
import org.cogchar.app.puma.vworld.PumaVirtualWorldMapper;
import org.cogchar.bundle.app.puma.PumaAppUtils;
import org.cogchar.name.thing.ThingCN;
import org.cogchar.render.sys.registry.RenderRegistryClient;
import org.cogchar.render.opengl.optic.CameraMgr;
import org.cogchar.render.sys.context.CogcharRenderContext;

import org.cogchar.render.sys.module.RenderGateway;

import org.cogchar.render.app.entity.CameraBinding;


import org.cogchar.render.trial.TrialContent;

import com.jme3.scene.Node;
import com.jme3.scene.CameraNode;
import com.jme3.asset.AssetManager;


/**
 * @author Stu B. <www.texpedient.com>
 */

public class DeicticVisualizer extends BasicDebugger {
		// Needing the CRC, this could use either  
		//  1) PumaAppUtils.GreedyHandleSet
				// This is doing a semi-JFluxy lookup style already.
		//  2) PAC from an activator's cheater's context
		//  3) PAC from a mediator callback.
		// Next step on this step will be fetching PumaRegClient from a JFlux pipeline.
		// The need for a CRC can also be resolved by extending existing
		// registry objects (so that HumanoidRenderMapper and other key high-level 
		// objects are easy to pick up, under appropriate conditions).  
		
	public void forceHeadCameraOntoSinbad() {
		Ident optVWorldSpecID = null;
		PumaAppUtils.GreedyHandleSet greedyHandles = new PumaAppUtils.GreedyHandleSet();
		PumaVirtualWorldMapper pvwm = greedyHandles.pumaRegClient.getVWorldMapper(optVWorldSpecID);
		forceHeadCameraOntoSinbad(pvwm);
	}
	public void forceHeadCameraOntoSinbad(RenderGateway rg) {
		// Note that a PumaVirtualWorldMapper is a RenderGateway!
		CogcharRenderContext crc = rg.getCogcharRenderContext();
		forceHeadCameraOntoSinbad(crc);
	}
	public void forceHeadCameraOntoSinbad(CogcharRenderContext crc) {
		Ident camID = new FreeIdent("uri:cameraConfig#sinbadHeadCam");
		float camPos[] = { 0, 0, 1};
		float camPointDir[] = {1, 1, 1};
		float displayRect[] = {0.0f, 0.3f, 0.7f, 1.0f};
		
		CameraConfig	hardHeadCC = new CameraConfig(camID, camPos, camPointDir, displayRect);
		Ident sinbadRobotID = new FreeIdent(ThingCN.CCRT_NS + "char_sinbad_88");
		String leftEyeBoneName = "Eye.L";
		RenderRegistryClient rrc = crc.getRenderRegistryClient();
		
		hardHeadCC.setAttachmentNodeParams(sinbadRobotID, leftEyeBoneName);
		CameraMgr camMgr = rrc.getOpticCameraFacade(null);
		camMgr.applyCameraConfig(hardHeadCC, rrc,  crc);
		
		CameraBinding	sinbadEyeCamBind = camMgr.getCameraBinding(camID);
		CameraNode sinbadEyeCamNode = sinbadEyeCamBind.getCameraNode();

		TrialContent trialCont = new TrialContent();
		AssetManager assetMgr = rrc.getJme3AssetManager(null);		
		Node sinbadVizPyrNode = trialCont.makeVisionPyramidNode(assetMgr, "sinbadEyeCam");
		sinbadEyeCamNode.attachChild(sinbadVizPyrNode);
		
		CameraBinding	defFlyByCamBind = camMgr.getDefaultCameraBinding();
		Node dfbVizPyrNode = trialCont.makeVisionPyramidNode(assetMgr, "defFlyBy");
		Node rootDeepNode = rrc.getJme3RootDeepNode(null);
		defFlyByCamBind.attachSceneNodeToCamera(dfbVizPyrNode, rootDeepNode);
	}
}
