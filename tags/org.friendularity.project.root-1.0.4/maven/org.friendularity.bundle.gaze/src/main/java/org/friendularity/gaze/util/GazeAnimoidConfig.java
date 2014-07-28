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
package org.friendularity.gaze.util;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 *
 * @author Owner
 */
public class GazeAnimoidConfig {
	/*
	// These core things all come from animoid_config.xml
	private		List<GazeStrategyCue>		myGazeStrategies;
	private		List<GazeJoint>				myGazeJoints;
	private		SightPort					myViewPort;
	private		FaceNoticeConfig			myFaceNoticeConfig;
	private		StereoGazeConfig			myStereoGazeConfig;
	private		FreckleMatchConfig			myFreckleMatchConfig;
	private		AnimationBlendConfig		myAnimationBlendConfig;

	public		Double						holdEntryNormDeg;
	public		Double						holdExitNormDeg;

	// These extra things don't come from XML, they are supplied during completeInit
	private		Robot						myMainRobot;
	private		Double						mySecPerFrame;
	private		Double						myFrameDurationSmoothingFactor;

	public void completeInit(Robot mainRobot, Integer msecPerFrame, Double frameDurSmoothFactor) {
		mySecPerFrame = msecPerFrame.doubleValue() / 1000.0;
		myFrameDurationSmoothingFactor = frameDurSmoothFactor;
		myMainRobot = mainRobot;
		for (GazeJoint gj: myGazeJoints) {
			if (myMainRobot != null) {
				Joint j = myMainRobot.getJointForOldLogicalNumber(gj.getLogicalJointID());
				gj.setJoint(j);
			}
		}
		if (myStereoGazeConfig != null) {
			myStereoGazeConfig.completeInit(this);
		}
		for (GazeStrategyCue gp : myGazeStrategies) {
			gp.completeInit(this);
		}
	}
	public Robot getMainRobot() {
		return myMainRobot;
	}

	public SightPort getViewPort() {
		return myViewPort;
	}
	public void setViewPort(SightPort vp) {
		myViewPort = vp;
	}
	public List<GazeJoint> getGazeJoints() {
		return myGazeJoints;
	}
	public Set<Joint> getAllGazeBoundJoints() {
		List<GazeJoint> allGazeJoints = getGazeJoints();
		Set<Joint> allGazeBoundJoints = new HashSet<Joint>();
		for (GazeJoint gj: allGazeJoints) {
			allGazeBoundJoints.add(gj.getJoint());
		}
		return allGazeBoundJoints;
	}
	public GazeJoint getGazeJointForLogicalNumber(Integer num) {
		for (GazeJoint gj: myGazeJoints) {
			if (gj.getLogicalJointID().equals(num)) {
				return gj;
			}
		}
		return null;
	}
	public FaceNoticeConfig getFaceNoticeConfig() {
		return myFaceNoticeConfig;
	}
	public StereoGazeConfig getStereoGazeConfig() {
		return myStereoGazeConfig;
	}
	public FreckleMatchConfig getFreckleMatchConfig() {
		return myFreckleMatchConfig;
	}
	public AnimationBlendConfig getAnimationBlendConfig() {
		return myAnimationBlendConfig;
	}
	public Double getSecondsPerFrame() {
		return mySecPerFrame;
	}
	public Double getFrameDurationSmoothingFactor() {
		return myFrameDurationSmoothingFactor;
	}
	public String toString() { 
		return "\nAnimoidConfig[viewPort=" + myViewPort 
				+ ",\n gazeStrategies=" + myGazeStrategies
				+ ",\n faceNoticeConfig=" + myFaceNoticeConfig
				+ ",\n stereoGazeConfig=" + myStereoGazeConfig
				+ ",\n hold-Entry/Exit-Thresh=" + holdEntryNormDeg + "/" + holdExitNormDeg
				+ ",\n freckleMatchConfig=" + myFreckleMatchConfig
				+ ",\n animationBlendConfig=" + myAnimationBlendConfig
				+ "]";
	}	
	*/
}
