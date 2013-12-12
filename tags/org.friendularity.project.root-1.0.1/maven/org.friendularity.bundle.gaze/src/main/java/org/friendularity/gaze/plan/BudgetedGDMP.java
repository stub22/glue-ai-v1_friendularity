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
import org.cogchar.animoid.calc.curvematrix.SDCACM_Interval;
import org.appdapter.bind.math.jscience.number.RealFuncs;
import org.jscience.mathematics.function.Polynomial;
import org.jscience.mathematics.function.Variable;
import org.jscience.mathematics.number.Real;

/**
 *
 * @author Stu B. <www.texpedient.com>
 */
public class BudgetedGDMP extends SharedDurationGDMP {
	private		Variable<Real>		myTargetPosVar;
	private		Polynomial<Real>	myTargetPosPoly;

	private		List<SharedDurationGDMP_Segment>		mySegments;
	
	public BudgetedGDMP(Integer numIntervals, Double planStartSec, Double maxDurSec, Double frameDurSec) {
		super(numIntervals, planStartSec, maxDurSec, frameDurSec);
		myTargetPosVar = new Variable.Local<Real>("targetPosDeg");
		myTargetPosPoly = Polynomial.valueOf(Real.ONE, myTargetPosVar);
	}

	@Override public void completeInitAfterJointsAdded() {
		super.completeInitAfterJointsAdded();
		mySegments = new ArrayList<SharedDurationGDMP_Segment>();
		List<SDCACM_Interval> intervals = myCACM.getIntervals();
		for (SDCACM_Interval interval: intervals) {
			SharedDurationGDMP_Segment seg = new SharedDurationGDMP_Segment(interval, myTargetPosPoly, this);
			mySegments.add(seg);
		}
	}
	protected void setGoalValue(double goalDeg) {
		RealFuncs.setVariableValue(myTargetPosVar, goalDeg);
	}
	@Override 	public  double computeTargetDistanceCost(double targetPosDeg) {
		return getIntegSqTargetDistanceForSharedDurs(targetPosDeg);
	}

	@Override	public double computePlanExecutionCost() {
		double sum = 0.0;
		sum += mySharedDurationPV.totalPenalties();
		return sum;
	}

	public double getIntegSqTargetDistanceForSharedDurs(double fixedTargetPosDeg) {
		// The target was already set for us in computeOptimalParams above.
		double sum = 0.0;
		int cursor = 0;
		for (SharedDurationGDMP_Segment s : mySegments) {
			double startTimeOffset = 0.0;
			double endTimeOffset = mySharedDurationPV.getValue(cursor++);
			double segVal = s.getIntegSquaredErrorForTimeRange(startTimeOffset, endTimeOffset);
			// theLogger.fine("segment definite integral for range[" + startTimeOffset
			//			+ "," + endTimeOffset + " = " + segVal);
			sum += segVal;
		}
		return sum;
	}
}
