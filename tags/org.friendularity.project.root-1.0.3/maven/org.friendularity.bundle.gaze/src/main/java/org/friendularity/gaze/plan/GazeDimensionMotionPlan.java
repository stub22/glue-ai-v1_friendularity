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

package org.friendularity.gaze.plan;


import java.util.ArrayList;
import java.util.List;
import org.friendularity.gaze.estimate.GazeJointStateSnap;

import org.cogchar.animoid.calc.optimize.MultiStepPJT;
import org.cogchar.animoid.calc.optimize.ParameterVector;
import org.friendularity.gaze.api.GazeJoint;
import org.jscience.mathematics.number.Number;
import org.jscience.mathematics.structure.Field;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Stu B. <www.texpedient.com>
 */
public abstract class GazeDimensionMotionPlan <RN extends Number<RN> & Field<RN>> {
	protected		static		boolean DUR_SHARED			= true;
	protected		static		boolean DUR_PER_JOINT		= false;
	private static Logger	theLogger = LoggerFactory.getLogger(GazeDimensionMotionPlan.class.getName());

	protected		List<GazeJointMotionPlan<RN>>		myJointPlans;
	protected		Integer								myIntervalsPerJoint;
	protected		Double							myPlanStartSec, myPlanMaxDurationSec, myFrameDurationSec;


	protected abstract void reconfigureUsingParams(double[] args, int startIdx);

	public abstract double computeTargetDistanceCost(double targetPosDeg);
	public abstract double computePlanExecutionCost();
	protected abstract GazeJointMotionPlan makeJoint(GazeJointStateSnap stat);
	public abstract void completeInitAfterJointsAdded();
	public abstract Integer getParamCountPerJoint();
	public abstract Integer getSharedParamCount();

	private void initVars(Integer numIntervals, Double frameDurSec) {
		myJointPlans = new ArrayList<GazeJointMotionPlan<RN>>();
		myIntervalsPerJoint = numIntervals;
		myFrameDurationSec = frameDurSec;
	}
	public GazeDimensionMotionPlan(Integer numIntervals, Double frameDurSec) {
		super();
		initVars(numIntervals, frameDurSec);
	}

	public GazeDimensionMotionPlan(Integer numIntervals, Double planStartSec, Double maxDurSec, Double frameDurSec) {
		super();
		initVars(numIntervals, frameDurSec);
		myPlanStartSec = planStartSec;
		myPlanMaxDurationSec = maxDurSec;
	}
	public void addJoint(GazeJointStateSnap stat) {
		// During unit tests we sometimes do not have joint object
		GazeJointMotionPlan gjmp = makeJoint(stat);
		myJointPlans.add(gjmp);
		gjmp.setTimeVars(myPlanStartSec, myPlanMaxDurationSec, myFrameDurationSec);
		gjmp.rebuildIntervals(myIntervalsPerJoint);
	}
	public GazeJointMotionPlan getMotionPlanForJoint(GazeJoint gj) {
		for (GazeJointMotionPlan gjmp : myJointPlans) {
			if (gjmp.getGazeJoint().equals(gj)) {
				return gjmp;
			}
		}
		return null;
	}
	public void updateJointKnownState(GazeJointStateSnap stat) {
		GazeJointMotionPlan gjmp = getMotionPlanForJoint(stat.getGazeJoint());
		if (gjmp == null) {
			throw new RuntimeException("Can't find existing motionPlan for joint: " + stat.getGazeJoint()
				+ " in my list of plans: " + myJointPlans);
		}
		gjmp.updateKnownJointState(stat);
	}
	public Integer getIndependentlyElasticDurationCount() {
		// The first duration is fixed, while the last is defined as "the rest".
		return myIntervalsPerJoint - 2;
	}
	public int getParamCountTotal(boolean abbrevDurationsPerJoint) {
		int numJoints = myJointPlans.size();
		int jointwiseParams = numJoints * getParamCountPerJoint();
		int sharedParams = this.getSharedParamCount();
		int totalParams = jointwiseParams + sharedParams;
		return totalParams;
	}

	protected int reconfigureStepTrajectories(double[] paramArray, int startCursor, boolean abbrevDursPerJoint) {
		// double jointPlanParams[] = new double[pcpj];
		int pcpj =  getParamCountPerJoint();
		int cursor = startCursor;
		for (GazeJointMotionPlan gjmp: myJointPlans) {
			MultiStepPJT mspjt = gjmp.getMultiStepPJT();
			int readCount = mspjt.readFromArray(paramArray, cursor, abbrevDursPerJoint);
			if (readCount != pcpj) {
				throw new RuntimeException("Expecting read of " + pcpj + " jointParams but got " + readCount);
			}
			gjmp.recalculateValues(abbrevDursPerJoint);
/* ***
			for (int j = 0; j < pcpj; j++) {
				jointPlanParams[j] = dimensionPlanParams[cursor + j];
			}
			gjmp.reconfigAccelLevelsAndDurations(jointPlanParams);
 *****/
			cursor += pcpj;
		}
		return cursor - startCursor;
	}


	// These values are all piecewise-differentiable in each of the plan parameters.
	// Pos is differentiable in time (2nd-order = parabolic + ramp) and all params.
	// vel is continuous in time and piecewise differentiable
	// (series of ramps), accel is piecewise constant in time(series of steps)
	


	public double computePlanTotalCost(double targetPosDeg, boolean sharedDurations,
				boolean printFlag) {
		double planExecutionCost = computePlanExecutionCost();
		double targetErrorCost = computeTargetDistanceCost(targetPosDeg);
		double totalCost = planExecutionCost + targetErrorCost;
		if (printFlag) {
			theLogger.trace("totalCost=" + totalCost
				 		+ ", targetErrorCost=" + targetErrorCost
				 		+ ", planExecCost=" + planExecutionCost);
		}
		return totalCost;
	}
	protected double getNominalRegularIntervalDuration() {
		// Remove the fixed length first interval, divide equally, and then
		// scale down by 20%, to leave room for the last duration to start off
		// as the longest.
		return 0.8 * (myPlanMaxDurationSec -  myFrameDurationSec)/(myIntervalsPerJoint - 1);
	}
	protected void resetDurationsVecToNominal(ParameterVector durVec) {
		double regularIntervalLength = getNominalRegularIntervalDuration();
		durVec.setValue(0, myFrameDurationSec);
		for (int i=1; i <  durVec.getLength(); i++) {
			durVec.setValue(i, regularIntervalLength);
		}
	}
	protected void resetAccelLevelVecToNominal(ParameterVector accVec) {
		accVec.setAllValues(0.0);
	}
	public Double getInitialPlannedAccelDegPSPSForJoint(GazeJoint gj) {
		Double result = null;
		GazeJointMotionPlan gjmp = getMotionPlanForJoint(gj);
		if (gjmp != null) {
			result = gjmp.getInitialAccelDegPSPS();
		}
		return result;
	}
	public double getPlannedVelDegPSForJointAtTimeOffset(GazeJoint gj, double timeOffset) {
		return 0.0;
	}
}
