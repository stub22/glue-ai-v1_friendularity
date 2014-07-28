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
import org.friendularity.gaze.estimate.GazeJointStateSnap;
import org.cogchar.animoid.calc.optimize.MultiStepPJT;
import org.cogchar.animoid.calc.optimize.ParameterVector;
import org.appdapter.bind.math.jscience.number.NumberFactory;
import org.friendularity.gaze.api.GazeJoint;
import org.friendularity.gaze.api.GazeJointStrategy;
import org.jscience.mathematics.function.Polynomial;
import org.jscience.mathematics.function.Term;
import org.jscience.mathematics.function.Variable;
import org.jscience.mathematics.number.Number;
import org.jscience.mathematics.structure.Field;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @param <RN> 
 * @author Stu B. <www.texpedient.com>
 */
public abstract class GazeJointMotionPlan <RN extends Number<RN> & Field<RN>>  {
	private static Logger	theLogger = LoggerFactory.getLogger(GazeJointMotionPlan.class.getName());

	GazeJoint							myGazeJoint;
	GazeJointStrategy					myStrategy;
	GazeJointStateSnap					myStat;

	Double								myPlanStartSec, myPlanDurSec, myFrameDurSec;
	MultiStepPJT						myMSPJT;
	NumberFactory<RN>					myNumberFactory;

	protected abstract void processDurationParameters(ParameterVector durPV);
	protected abstract void processAccelParamsAndKnownState(ParameterVector accelPV,
				GazeJointStateSnap knownState);
	protected abstract double getInitialAccelDegPSPS();

	// We fix the length of the first interval because this is the
	// planning decision to be turned into immediate action.
	public GazeJointMotionPlan(GazeJointStrategy gjs, NumberFactory<RN> numFact) {
		myStrategy = gjs;
		myGazeJoint = myStrategy.getGazeJoint();
		myNumberFactory = numFact;
	}
	public GazeJoint getGazeJoint() {
		return myGazeJoint;
	}
	public void setTimeVars(Double planStartSec, Double planDurSec, Double frameDurSec) {
		myPlanStartSec = planStartSec;
		myPlanDurSec = planDurSec;
		myFrameDurSec = frameDurSec;
		// updateInternalTimeConfig();
	}
	public MultiStepPJT getMultiStepPJT() {
		return myMSPJT;
	}
	public void rebuildIntervals(Integer numIntervals) {
		myMSPJT = new MultiStepPJT(myNumberFactory);
		myMSPJT.setStepCount(numIntervals);
		// updateInternalTimeConfig();
	}

	public void updateKnownJointState(GazeJointStateSnap stat) {
		myStat = stat;
		/*
		GazeJointMotionPlanInterval firstI = this.getFirstInterval();
		firstI.setStartVelDegPS(stat.myVelDegPerSec);
		firstI.setStartPosDeg(stat.myPosDegOffCenter);
		*/
	}
	public void recalculateValues(boolean abbrevDursPerJoint) {
		ParameterVector accelPV = myMSPJT.getLevelPV();
		ParameterVector durationPV = myMSPJT.getDurationPV();
		processDurationParameters (durationPV);
		processAccelParamsAndKnownState(accelPV, myStat);
	}

	public String toString() {
		return "GazeJointMotionPlan[stat=" + myStat 
				+ ",\n strategy=" + myStrategy
				+ ", planStart=" + myPlanStartSec
				+ ", planDur=" + myPlanDurSec
				+ ", frameDur=" + myFrameDurSec
				+ ",\n" + getDetailedPlanDescription()
				+ "]";
	}
	public String getDetailedPlanDescription() {
		return  "mspjt=" + myMSPJT;
	}
	protected static void fixAndPunishDurationParameters(ParameterVector durPV,
				double frameDurSec, double planDurSec, double indPenWeight, double sumPenWeight) {
		durPV.setAllPenalties(0.0, false);
		durPV.myBulkPenalty = 0.0;
		int icnt = durPV.getLength();
		durPV.setValue(0, frameDurSec);
		durPV.setValue(icnt - 1, frameDurSec);
		for (int i=1; i < icnt -1; i++) {
			double durVal = durPV.getValue(i);
			if (durVal < frameDurSec) {
				// Supplied duration is less than one frame.
				double deficit = frameDurSec - durVal;
				if (durVal < 0) {
					// Big extra penalty for going under 0.
					deficit += 100.0;
				}
				if (durVal < 0.1 * frameDurSec) {
					durPV.setValue(i, 0.1 * frameDurSec);
				}
				durPV.setPenalty(i, indPenWeight * deficit, true);
			}
		}
		double fullSum = durPV.sumValues();
		if (fullSum > planDurSec) {
			double excess = fullSum - planDurSec;
			durPV.myBulkPenalty = sumPenWeight * excess;
		} else {
			double deficit = planDurSec - fullSum;
			durPV.setValue(icnt - 1, frameDurSec + deficit);
		}
	}
	public Polynomial<RN> getInstantExecCostPolyForStepCurve(int stepIndex) {
		ConstAccelCurve<RN> stepCurve = getStepCurve(stepIndex);
		Polynomial<RN> posPoly = stepCurve.getCurvePoly();
		Polynomial<RN> velPoly = stepCurve.getVelocityCurve();
		Polynomial<RN> accPoly = stepCurve.getAccelCurve();

		RN posWeight = stepCurve.makeNumber(myStrategy.getPosStrainCostWeight());
		RN velWeight = stepCurve.makeNumber(myStrategy.getWorkFuelCostWeight());
		RN accWeight = stepCurve.makeNumber(myStrategy.getAccelStrainCostWeight());

		Variable<RN> posVar = new Variable.Local<RN>("pos");
		Variable<RN> velVar = new Variable.Local<RN>("vel");
		Variable<RN> accVar = new Variable.Local<RN>("acc");
		
		Term posSqTerm = Term.valueOf(posVar, 2);
		Term velSqTerm = Term.valueOf(velVar, 2);
		Term accSqTerm = Term.valueOf(accVar, 2);		

		Polynomial<RN> weighPosSqPoly = Polynomial.valueOf(posWeight, posSqTerm);
		Polynomial<RN> weighVelSqPoly = Polynomial.valueOf(velWeight, velSqTerm);
		Polynomial<RN> weighAccSqPoly = Polynomial.valueOf(accWeight, accSqTerm);

		Polynomial<RN> compPosCostPoly = weighPosSqPoly.compose(posPoly);
		Polynomial<RN> compVelCostPoly = weighVelSqPoly.compose(velPoly);
		Polynomial<RN> compAccCostPoly = weighAccSqPoly.compose(accPoly);

		Polynomial<RN> totalInstCostPoly = compPosCostPoly.plus(compVelCostPoly).plus(compAccCostPoly);

		return totalInstCostPoly;
	}
	public abstract ConstAccelCurve<RN> getStepCurve(int stepIndex);
}
