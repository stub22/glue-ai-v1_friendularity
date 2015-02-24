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

import org.cogchar.animoid.calc.curve.ConstAccelCurve;
import org.cogchar.animoid.calc.curvematrix.ConstAccelCurveSequence;
import org.friendularity.gaze.estimate.GazeJointStateSnap;
import org.cogchar.animoid.calc.optimize.ParameterVector;


import org.appdapter.bind.math.jscience.number.NumberFactory;
import org.friendularity.gaze.api.GazeJointStrategy;
import org.jscience.mathematics.number.Number;
import org.jscience.mathematics.structure.Field;

/**
 *
 * @param <RN>
 * @author Stu B. <www.texpedient.com>
 */
public class SharedDurationGJMP<RN extends Number<RN> & Field<RN>> extends GazeJointMotionPlan<RN> {

	ConstAccelCurveSequence<RN>				myAccelCurveSeq;

	public SharedDurationGJMP(GazeJointStrategy gjs, NumberFactory<RN> numFact) {
		super(gjs, numFact);
	}
	public void setAccelCurveSeq(ConstAccelCurveSequence seq) {
		myAccelCurveSeq = seq;
	}
	public ConstAccelCurve<RN> getStepCurve(int stepIndex) {
		return myAccelCurveSeq.getStepCurve(stepIndex);
	}
	protected void processDurationParameters(ParameterVector durPV) {
		// myAccelCurveSeq.setDurationParams(durPV);
	}
	protected void processAccelParamsAndKnownState(ParameterVector accelPV,
				GazeJointStateSnap knownState) {
		double prevIntervalEndPosDeg = knownState.getPosInternalDegOffCenter();
		double prevIntervalStepEndVelDegPS = knownState.getInternalVelDegPerSec();

		myAccelCurveSeq.setAccelParams(accelPV);
		RN initPosNum = myNumberFactory.makeNumberFromDouble(prevIntervalEndPosDeg);
		RN initVelNum = myNumberFactory.makeNumberFromDouble(prevIntervalStepEndVelDegPS);
		myAccelCurveSeq.setInitialConditions(initPosNum, initVelNum);
	}
	@Override protected double getInitialAccelDegPSPS() {
		return myAccelCurveSeq.getFirstStepCurve().getAccelAtCurrentState().doubleValue();
	}

	@Override public String getDetailedPlanDescription() {
		return myAccelCurveSeq.dumpMotionPlan(11, this.myPlanDurSec);
	}



}
