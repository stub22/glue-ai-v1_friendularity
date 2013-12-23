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

package org.friendularity.gmteach.impl.visual;

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

import org.cogchar.render.app.humanoid.HumanoidRenderContext;
import org.cogchar.render.model.humanoid.HumanoidFigureManager;

import java.util.Map;
import java.util.HashMap;

import com.jme3.math.Quaternion;

import org.friendularity.gmteach.api.west.SelfEstimate;
import org.cogchar.render.goody.dynamic.VizShapeGroup;

/**
 * @author Stu B. <www.texpedient.com>
 */

public class DeicticVisualizer extends BaseVisualizer<SelfEstimate> {
		// Needing the CRC, this could use either  
		//  1) PumaAppUtils.GreedyHandleSet
				// This is doing a semi-JFluxy lookup style already.
		//  2) PAC from an activator's cheater's context
		//  3) PAC from a mediator callback.
		// Next step on this step will be fetching PumaRegClient from a JFlux pipeline.
		// The need for a CRC can also be resolved by extending existing
		// registry objects (so that HumanoidRenderMapper and other key high-level 
		// objects are easy to pick up, under appropriate conditions).  
		

	private	TrialContent		myTrialContent;
	private boolean				myFlag_NeutralizationComplete = false;
	private CameraNode			mySinbadEyeCamNode;	
	private Map<String, Node>	myAttachNodesByName = new HashMap<String, Node>();
	
	String[] interestingBoneNames = 
		{"Eye.R", "Head", "Root"}; //, "Chest", "Hand.R", "Hand.L", "IndexFingerDist.R",
				// "Toe.L", "Toe.R"};	
	
	public DeicticVisualizer(BaseVisualizer<?> otherViz) {
		super(otherViz);
	}
	public void connectToTrialContent(TrialContent tc) {
		myTrialContent = tc;
	}
	private void ensureSetup() {
		if (myTrialContent == null) {
			myTrialContent = new TrialContent();
		}
	}
	public void forceHeadCameraOntoSinbad() {
		ensureSetup();
		Ident camID = new FreeIdent("uri:cameraConfig#sinbadHeadCam");
		float camPos[] = { 0, 0, 1};
		float camPointDir[] = {1, 1, 1};
		float displayRect[] = {0.0f, 0.3f, 0.7f, 1.0f};
		
		CameraConfig	hardHeadCC = new CameraConfig(camID, camPos, camPointDir, displayRect);
		Ident sinbadRobotID = new FreeIdent(ThingCN.CCRT_NS + "char_sinbad_88");
		String leftEyeBoneName = "Eye.L";
		RenderRegistryClient rrc = getRenderRegistryClient();
		
		hardHeadCC.setAttachmentNodeParams(sinbadRobotID, leftEyeBoneName);
		CameraMgr camMgr = rrc.getOpticCameraFacade(null);
		camMgr.applyCameraConfig(hardHeadCC, rrc);
		
		CameraBinding	sinbadEyeCamBind = camMgr.getCameraBinding(camID);
		CameraNode sinbadEyeCamNode = sinbadEyeCamBind.getCameraNode();

		// World transforms have not yet been calculated.  Remember this node for later.
		mySinbadEyeCamNode = sinbadEyeCamNode;
		
		Node sinbadVizPyrNode = myTrialContent.makePointerCone(rrc, "sinbadEyeCam");
		sinbadEyeCamNode.attachChild(sinbadVizPyrNode);
	}

	
	public void neutralizeEyeCamRotation(CameraNode eyeCamNode) { 
		
		// When does the world translation actually become available? 
		
		/* Another approach is to use Node.lookAt, but note this bug from the
		 * Javadoc, reportedly fixed after our last JME3 snapshot:
		 * 
		 * lookAt is a convenience method for auto-setting the local rotation 
		 * based on a position in world space and an up vector. It computes 
		 * the rotation to transform the z-axis to point onto 'position' and 
		 * the y-axis to 'up'. Unlike Quaternion.lookAt(com.jme3.math.Vector3f, 
		 * com.jme3.math.Vector3f) this method takes a world position to look at 
		 * and not a relative direction. Note : 28/01/2013 this method has been 
		 * fixed as it was not taking into account the parent rotation. This was 
		 * resulting in improper rotation when the spatial had rotated parent nodes. 
		 * This method is intended to work in world space, so no matter what parent 
		 * graph the spatial has, it will look at the given position in world space.
		 */
		Quaternion eyeCamWorldRot = eyeCamNode.getWorldRotation();
		//  (0.5573061, 0.44985783, 0.49981913, 0.4870509)
		getLogger().info("EyeCam world rotation: {}", eyeCamWorldRot);
		Quaternion fixupRot = eyeCamWorldRot.inverse();
		eyeCamNode.setLocalRotation(fixupRot);	
		myFlag_NeutralizationComplete = true;
	}
	public void putVizPyramidOnDefaultCam() {	
		ensureSetup();
//		CogcharRenderContext crc = myRenderGateway.getCogcharRenderContext();
		RenderRegistryClient rrc = getRenderRegistryClient();
		putVizPyramidOnDefaultCam(rrc);
	}
	public void putVizPyramidOnDefaultCam(RenderRegistryClient rrc) {	
		CameraMgr camMgr = rrc.getOpticCameraFacade(null);
		AssetManager assetMgr = rrc.getJme3AssetManager(null);	
		CameraBinding	defFlyByCamBind = camMgr.getDefaultCameraBinding();
		Node dfbVizPyrNode = myTrialContent.makePointerCone(rrc, "defFlyBy");
		Node rootDeepNode = rrc.getJme3RootDeepNode(null);
		defFlyByCamBind.attachSceneNodeToCamera(dfbVizPyrNode, rootDeepNode);
		
	}
	

	
	private void registerBoneNodesOfInterest() { 
		// HumanoidRenderContext hrc = (HumanoidRenderContext) crc;
		HumanoidFigureManager hfm = getHumanoidFigureManager(); // hrc.getHumanoidFigureManager();
		Ident sinbadRobotID = new FreeIdent(ThingCN.CCRT_NS + "char_sinbad_88");
		for (String boneName : interestingBoneNames) {
			Node attachNode = hfm.findHumanoidBoneAttachNode(sinbadRobotID, boneName);
			myAttachNodesByName.put(boneName, attachNode);
		}
	}
	
	private void putDeicticPointingRayOnBone(String boneName) { 
		RenderRegistryClient rrc = getRenderRegistryClient();
		Node boneAttachNode = myAttachNodesByName.get(boneName);
		
		AssetManager assetMgr = rrc.getJme3AssetManager(null);		
		Node pointNode = myTrialContent.makePointerCone(rrc, "viz_" + boneName);
		boneAttachNode.attachChild(pointNode);		
	}
	
	public void setupNiftyPointingRays() { 
		ensureSetup();
		registerBoneNodesOfInterest();
		for (String boneName : interestingBoneNames) {
			putDeicticPointingRayOnBone(boneName);
		}
	}
	public void doUpdate(RenderRegistryClient rrc, float tpf)	{
		if ((!myFlag_NeutralizationComplete) && (mySinbadEyeCamNode != null)) {
			neutralizeEyeCamRotation(mySinbadEyeCamNode);
		}
	}

	@Override public VizShapeGroup getShapeGroup() {
		throw new UnsupportedOperationException("Not supported yet.");
	}

	@Override public void ensureDisplayed_onRendThrd(SelfEstimate te, float timePerFrame) {
		throw new UnsupportedOperationException("Not supported yet."); 
	}

	@Override public void updateDisplay_onRendThrd(SelfEstimate te, float timePerFrame) {
		doUpdate(null, timePerFrame);
	}
}
/**
 * 
 		<bone id="46" name="Eye.L">
			<position x="-0.269563" y="0.636877" z="-0.660801"/>
			<rotation angle="2.234008">
				<axis x="-0.567339" y="-0.527655" z="0.632224"/>
			</rotation>
		</bone>
		<bone id="56" name="Eye.R">
			<position x="0.269568" y="0.636878" z="-0.660801"/>
			<rotation angle="2.511607">
				<axis x="-0.395561" y="-0.595988" z="0.698806"/>
			</rotation>
		</bone>
 * 
 * 
 In some animations we see
 * for L
 * 							<rotate angle="0.002762">
								<axis x="0.988286" y="-0.120859" z="-0.093184"/>
							</rotate>
							* 	<axis x="0.927962" y="-0.337441" z="0.158176"/>
for R
					<axis x="0.843820" y="-0.120862" z="-0.522838"/>* 
					* 		<axis x="-0.842785" y="0.004418" z="0.538232"/>
							* 
 */ 