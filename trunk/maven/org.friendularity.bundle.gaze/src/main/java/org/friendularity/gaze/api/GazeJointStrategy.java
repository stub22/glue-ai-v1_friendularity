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

/**
 * @author Stu B. <www.texpedient.com>
 */
public class GazeJointStrategy {

	private		Integer		logicalJointID;
	
	private		GazeJoint	myGazeJoint;

	// This applies only when strategy is JUMPY
	public		Double		flatMotionWeight;

	// These 3 apply only when strategy is RAMPY
	public		Double		rampVelMaxDPS;
	public		Double		rampAccelMaxDPSPS;
	public		Double		rampDecelMaxDPSPS;

	// These 2 apply only when strategy is HOLD_AND_RECENTER
	public		Double		recenterSlackDeg;
	public		Double		recenterMaxVelDPS;
	
/***********  Begin fancy next-gen optimization parameters*/
	// All costs are measured in degrees so immune to changes in ROM physical extent.

	// "Fuel" cost every (degree-per-sec)-squared of work.
	// The mass in context of an energy/fuel cost.  All that matters is the
	// max vel reached, so this inhibits max velocity, not acceleration.
	private		Double		workFuelCostWeight;

	// "Strain" cost every (degree-per-sec-per-sec)-squared of accel-squared-integ
	// with accel measured in degrees.
	// Similar to fragility, encourages us to keep accel down (must be at least squared)
	private		Double		accelStrainCostWeight;	// Cost every degree-per-sec-per-sec of accel

	// Position "strain" should be based on closeness to edge of ROM (with center
	// at ROM center or a nominal center like default point?)
	private		Double		posStrainCostWeight; // displacement from nominal per-deg-per-sec

	private		Double		posStrainCenterROM;
/***********  End fancy next-gen optimization parameters*/


	public void completeInit(AnimoidConfig animConf) {
		// XStream bypasses regular initializers, so can't do this inline above.
		
	//	myGazeJoint = animConf.getGazeJointForLogicalNumber(logicalJointID);
	}
	public GazeJoint getGazeJoint() {
		return myGazeJoint;
	}
	
	public int getLogicalJointID() {
    	return logicalJointID;
    }

	public String toString() {
		return "\nGazeJointLink[" 
				+ "\ngazeJoint=" + myGazeJoint
				+ "]";
	}



	/**
	 * @return weight
	 */
	public Double getWorkFuelCostWeight() {
		return workFuelCostWeight;
	}

	/**
	 * @param weight
	 */
	public void setWorkFuelCostWeight(Double weight) {
		workFuelCostWeight = weight;
	}
	/**
	 * @return accelStrainCostWeight
	 */
	public Double getAccelStrainCostWeight() {
		return accelStrainCostWeight;
	}
	/**
	 * @param weight
	 */
	public void setAccelStrainCostWeight(Double weight) {
		accelStrainCostWeight = weight;
	}
	/**
	 * @return posStrainCostWeight
	 */
	public Double getPosStrainCostWeight() {
		return posStrainCostWeight;
	}

	/**
	 * @param weight
	 */
	public void setPosStrainCostWeight(Double weight) {
		posStrainCostWeight = weight;
	}
	/**
	 * @return center point in ROM coords [0.0,1.0]
	 */
	public Double getPosStrainCenterROM() {
		return posStrainCenterROM;
	}

	/**
	 * @param centerPosROM - center point in ROM coords [0.0,1.0]
	 */
	public void setPosStrainCenterROM(Double centerPosROM) {
		posStrainCenterROM = centerPosROM;
	}

}
