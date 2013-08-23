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
/**
 * @author Stu B. <www.texpedient.com>
 */
public class GlanceStrategy {
	// When the face center is within this many pixels of FOV center, 
	// we are "staring", and a timer starts.
	private 	Float		stareNoticeRadiusPix;
	// If the face is lost or uncentered for more than this long, 
	// then the timer is cleared.
	private 	Float		stareResetDelaySec;
	// When the timer reaches this duration, "glancing" becomes an option.
	private		Float		stareHoldMinSec;
	// We should always glance before this.
	private		Float		stareHoldMaxSec;
	// The thing we glance at should be at least this far from the focused face.
	private		Float		glanceTargetMinRadiusPix;
	// We should not return to the original face before this.
	private		Float		glanceHoldMinSec;
	// We should always return before this.
	private		Float		glanceHoldMaxSec;
	
	private		Integer		escapeThrustFrames;
	
	public void completeInit() {
		if (escapeThrustFrames == null) {
			escapeThrustFrames = 1;
		}
	}
	public String toString() { 
		return "GlanceStrategy=["
			+ "\nstareNoticeRadiusPix="  + stareNoticeRadiusPix
			+ "\nstareResetDelaySec=" + stareResetDelaySec
			+ "\nstareHoldMinSec=" + stareHoldMinSec
			+ "\nstareHoldMaxSec=" + stareHoldMaxSec
			+ "\nglanceTargetMinRadiusPix=" + glanceTargetMinRadiusPix
			+ "\nglanceHoldMinSec=" + glanceHoldMinSec
			+ "\nglanceHoldMaxSec=" + glanceHoldMaxSec + "]";
	}

	public Float getStareNoticeRadiusPix() {
    	return stareNoticeRadiusPix;
    }

	public void setStareNoticeRadiusPix(Float stareNoticeRadiusPix) {
    	this.stareNoticeRadiusPix = stareNoticeRadiusPix;
    }

	public Float getStareResetDelaySec() {
    	return stareResetDelaySec;
    }

	public void setStareResetDelaySec(Float stareResetDelaySec) {
    	this.stareResetDelaySec = stareResetDelaySec;
    }

	public Float getStareHoldMinSec() {
    	return stareHoldMinSec;
    }

	public void setStareHoldMinSec(Float stareHoldMinSec) {
    	this.stareHoldMinSec = stareHoldMinSec;
    }

	public Float getStareHoldMaxSec() {
    	return stareHoldMaxSec;
    }

	public void setStareHoldMaxSec(Float stareHoldMaxSec) {
    	this.stareHoldMaxSec = stareHoldMaxSec;
    }

	public Float getGlanceTargetMinRadiusPix() {
    	return glanceTargetMinRadiusPix;
    }

	public void setGlanceTargetMinRadiusPix(Float glanceTargetMinRadiusPix) {
    	this.glanceTargetMinRadiusPix = glanceTargetMinRadiusPix;
    }

	public Float getGlanceHoldMinSec() {
    	return glanceHoldMinSec;
    }

	public void setGlanceHoldMinSec(Float glanceHoldMinSec) {
    	this.glanceHoldMinSec = glanceHoldMinSec;
    }

	public Float getGlanceHoldMaxSec() {
    	return glanceHoldMaxSec;
    }

	public void setGlanceHoldMaxSec(Float glanceHoldMaxSec) {
    	this.glanceHoldMaxSec = glanceHoldMaxSec;
    }
	public Integer getEscapeThrustFrames() {
    	return escapeThrustFrames;
    }
	public void setEscapeThrustFrames(Integer escapeThrustFrames) {
    	this.escapeThrustFrames = escapeThrustFrames;
    }
}
