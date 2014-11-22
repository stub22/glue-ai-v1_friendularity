/*
 *  Copyright 2011 by The Cogchar Project (www.cogchar.org).
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
package org.friendularity.gaze.api;


import org.cogchar.api.animoid.config.bonus.AnimoidConfig;
import org.cogchar.api.animoid.protocol.Joint;
import org.cogchar.api.animoid.world.WorldJoint;

/**
 * @author Stu B. <www.texpedient.com>
 */
public class StereoGazeConfig {
	public enum Orientation {
		CAMERA_IN_LEFT_EYE,
		CAMERA_IN_RIGHT_EYE
	}
	public Orientation	orientation;
	// Camera joint must be a gaze joint
	public Integer					cameraJointLogicalID;
	// Partner joint may NOT be a gaze joint
	public Integer					partnerJointLogicalID;
	public GazeJoint.Direction		partnerJointDirection;
	// We need to know partner's range - MEASUURE, DON'T GUESS!
	public Double					partnerJointRangeOfMotionDegrees;
	// defaultVergenceDegrees - SHOULD BE MEASURED, NOT CHOSEN ARBITRARILY!!!

	// When both joints are at their default positions, this is the apparent
	// angle between the rays from the eyes.
	// 0 = parallel,
	// +x = converged to focal point, acute angle between rays is x (0 -> 90)
	// -x = diverged out Marty Feldman style.  (0 -> -90)
	public Double					defaultVergenceDegrees;
    // When a face is this many pixels wide, our vergence angle is 0
    public Double					defaultFaceWidthPixels;
    //  Our vergence angle will be:
    //  vergenceSlope * (faceWidth - defaultFaceWidth)
    public Double					vergenceSlope;

	private transient GazeJoint		myCameraGazeJoint;
	private transient WorldJoint	myPartnerWorldJoint;

	private class PartnerWorldJoint extends WorldJoint {
		public PartnerWorldJoint(Joint j) {
			super.logicalJointID = j.oldLogicalJointNumber;
			super.myJoint = j;
			super.rangeOfMotionDegrees = partnerJointRangeOfMotionDegrees;
		}
		@Override public boolean isWorldSenseInverted() {
			return partnerJointDirection != GazeJoint.Direction.RIGHT;
		}
	}
	public void completeInit(AnimoidConfig ac) {
		// myCameraGazeJoint = ac.getGazeJointForLogicalNumber(cameraJointLogicalID);
		Joint partnerJoint = ac.getMainRobot().getJointForOldLogicalNumber(partnerJointLogicalID);
		if (partnerJoint == null) {
			throw new RuntimeException("Cannot locate GazePartnerJoint on logicalID: " + partnerJointLogicalID);
		}
		myPartnerWorldJoint = new PartnerWorldJoint(partnerJoint);
	}
	public GazeJoint getCameraGazeJoint() {
		return myCameraGazeJoint;
	}
	public WorldJoint getPartnerWorldJoint() {
		return myPartnerWorldJoint;
	}

	public double getVergencePartnerWorldAngleMultiplier() {
		// If camera is in left eye, then "increasing vergence" means that
		// the partner (right eye) is moving left, which is negative in
		// world space.  
		return (orientation == Orientation.CAMERA_IN_LEFT_EYE) ? -1.0 : 1.0;
	}
	@Override public String toString() {
		return "StereoGazeConfig["
			+ "\ncameraJointLogicalID=" + cameraJointLogicalID
			+ "\npartnerJointLogicalID=" + partnerJointLogicalID
			+ "\npartnerJointDirection=" + partnerJointDirection
			+ "\npartnerJointRangeOfMotionDegrees=" + partnerJointRangeOfMotionDegrees
			+ "\ndefaultVergenceDegrees=" + defaultVergenceDegrees
			+ "\ndefaultFaceWidthPixels=" + defaultFaceWidthPixels
			+ "\nvergenceSlope=" + vergenceSlope
			+ "]";
	}	
}
