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


import org.friendularity.gaze.estimate.GazeJointStateSnap;
import org.cogchar.animoid.calc.curvematrix.ConstAccelCurveSequence;
import org.cogchar.animoid.calc.curvematrix.SharedDurationCACM;
import org.cogchar.animoid.calc.optimize.ParameterVector;
import org.appdapter.bind.math.jscience.number.RealFuncs;
import org.jscience.mathematics.function.Polynomial;
import org.jscience.mathematics.number.Real;

/**
 * @author Stu B. <www.texpedient.com>
 */
public abstract class SharedDurationGDMP extends GazeDimensionMotionPlan<Real> {
	protected		SharedDurationCACM<Real>		myCACM;
	protected		ParameterVector					mySharedDurationPV;

	public SharedDurationGDMP(Integer numIntervals, Double planStartSec, Double maxDurSec, Double frameDurSec) {
		super(numIntervals, planStartSec, maxDurSec, frameDurSec);
		myCACM = new SharedDurationCACM<Real>(RealFuncs.getRealNumberFactory());
	}
	@Override protected SharedDurationGJMP makeJoint(GazeJointStateSnap stat) {
		// During unit tests we sometimes do not have joint object
		String jointName = stat.getShortDescriptiveName();
		ConstAccelCurveSequence seq = myCACM.addEmptySequenceForName(jointName);
		SharedDurationGJMP<Real> gjmp = new SharedDurationGJMP<Real>(stat.getGazeJointStrategy(),
			RealFuncs.getRealNumberFactory());
		gjmp.setAccelCurveSeq(seq);
		return gjmp;
	}
	@Override public void completeInitAfterJointsAdded() {
		buildSharedDurationFrames();
	}
	private void buildSharedDurationFrames() {
		for (int i=0; i < myIntervalsPerJoint; i++) {
			myCACM.appendInterval();
		}
		mySharedDurationPV = new ParameterVector(null);
		mySharedDurationPV.setLength(myIntervalsPerJoint);
	}
	@Override public void reconfigureUsingParams(double[] paramArray, int ourStartIdx) {
		int expectedParamCount = getParamCountTotal(false);
		ParameterVector		durPV = mySharedDurationPV;
		int elasticDurCount = getIndependentlyElasticDurationCount();
		durPV.readValuesFromArray(paramArray, ourStartIdx, 1, elasticDurCount);
		int cursor = ourStartIdx + elasticDurCount;
		int jointwiseParamsRead = reconfigureStepTrajectories(paramArray, cursor, false);
		GazeJointMotionPlan.fixAndPunishDurationParameters(durPV, myFrameDurationSec, myPlanMaxDurationSec, 100000.0, 50000.0);
		myCACM.setDurations(durPV);
		myCACM.propagateEndpointConditions();
	}
	@Override public Integer getParamCountPerJoint() {
		return myIntervalsPerJoint;
	}
	@Override public Integer getSharedParamCount() {
		return  getIndependentlyElasticDurationCount();
	}
	public Polynomial<Real> getInstantExecCostPolyForInterval(int intervalIndex) {
		Polynomial<Real> sumPoly = null;
		for (GazeJointMotionPlan<Real> gjmp: myJointPlans) {
			Polynomial<Real> stepCost = gjmp.getInstantExecCostPolyForStepCurve(intervalIndex);
			if (sumPoly == null) {
				sumPoly = stepCost;
			} else {
				sumPoly = sumPoly.plus(stepCost);
			}
		}
		return sumPoly;
	}
}
