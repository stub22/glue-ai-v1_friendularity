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
package org.friendularity.impl.visual;

import org.cogchar.render.sys.registry.RenderRegistryClient;
import com.jme3.asset.AssetManager;
import com.jme3.scene.Node;

import org.cogchar.render.app.humanoid.HumanoidRenderContext;
import org.cogchar.blob.emit.RenderConfigEmitter;
import org.friendularity.api.west.ThingEstimate;

import org.friendularity.api.west.WorldEstimate;
import org.friendularity.vworld.MeshTest;

import org.cogchar.render.trial.TrialContent;
import org.cogchar.render.trial.TrialCameras;
import org.cogchar.render.trial.TempMidiBridge;

import org.cogchar.render.opengl.optic.ViewportFacade;


/**
 *
 * @author Stu B22 <stub22@appstract.com>
 */
public class BonusVisualizer extends ShapeAnimVisualizer<WorldEstimate> {

	TrialContent	myTrialContent;
	
	public DeicticVisualizer	myDVHackForUpdate;
	
	public BonusVisualizer(HumanoidRenderContext hrc) {
		super(hrc);
	}


	public void makeBonusMeshes() {
		RenderRegistryClient rrc = getRenderRegistryClient();
		AssetManager amgr = rrc.getJme3AssetManager(null);
		Node rootNode = rrc.getJme3RootDeepNode(null);
		RenderConfigEmitter rce = getConfigEmitter();
		MeshTest mt = new MeshTest();
		mt.makeStuff(amgr, rootNode);
		initTrialContent();
	}

	public void initTrialContent() { 
		TempMidiBridge tmb = new TempMidiBridge();
		myTrialContent = new TrialContent();
		RenderRegistryClient rrc = getRenderRegistryClient();
		
		Node rootDeepNode = rrc.getJme3RootDeepNode(null);
		ViewportFacade vf = rrc.getOpticViewportFacade(null);
		AssetManager assetMgr = rrc.getJme3AssetManager(null);
		
		Node guiNode = rrc.getJme3RootOverlayNode(null);
		
		// trialCont.shedLight_onRendThread(crc);
		myTrialContent.initContent3D_onRendThread(rrc, rootDeepNode);
		
		// Camera-viewports are placed in the screen coordinate system, so we might consider them to be a kind
		// of 2-D content.  They are part of that layout, anyhoo.
		myTrialContent.initContent2D_onRendThread(rrc, guiNode, assetMgr);
		// trialCont.attachMidiCCs(tmb);  // was protected access
		// CogcharRenderContext crc = getRenderContext();
		// TrialCameras tcam = new TrialCameras();
		// tcam.setupCamerasAndViews(rrc, crc, trialCont);
		// tcam.attachMidiCCs(myTMB);		
	}
	public TrialContent getTrialContent() { 
		return myTrialContent;
	}
	@Override public void updateDisplay_onRendThrd(WorldEstimate we, float timePerFrame) {
		super.updateDisplay_onRendThrd(we, timePerFrame);
		if (myTrialContent != null) {
			RenderRegistryClient rrc = getRenderRegistryClient();
			myTrialContent.doUpdate(rrc, timePerFrame);
		}
		if (myDVHackForUpdate != null) {
			RenderRegistryClient rrc = getRenderRegistryClient();
			myDVHackForUpdate.doUpdate(rrc, timePerFrame);
		}
	}

}
