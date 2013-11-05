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


import org.cogchar.animoid.calc.curvematrix.RampingFramedCACM;
import org.cogchar.animoid.calc.curvematrix.RampingFramedCurveSeq;
import org.friendularity.gaze.estimate.GazeJointStateSnap;

import org.cogchar.api.animoid.protocol.Frame;
import org.cogchar.api.animoid.protocol.JVFrame;
import org.cogchar.api.animoid.protocol.Joint;
import org.cogchar.api.animoid.world.SummableWJTrajectoryList;
import org.cogchar.api.animoid.world.SummableWorldJointList;
import org.cogchar.api.animoid.world.WorldGoalPosition;
import org.appdapter.bind.math.jscience.number.NumberFactory;
import org.cogchar.platform.util.TimeUtils;
import org.friendularity.gaze.api.GazeDimension;
import org.friendularity.gaze.api.GazeJoint;

import org.jscience.mathematics.number.Number;
import org.jscience.mathematics.structure.Field;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Stu B. <www.texpedient.com>
 */

public class SimpleGazeDimensionPlan<RN extends Number<RN> & Field<RN>> {
	private static Logger		theLogger = LoggerFactory.getLogger(SimpleGazeDimensionPlan.class.getName());
	private		NumberFactory<RN>			myNumberFactory;

	private		RampingFramedCACM<RN>		myCurveMatrix;

	private		SummableWorldJointList<GazeJoint>					myWorldJointList;
	private		SummableWJTrajectoryList<SimpleGazeJointTrajectory>	myTrajList;

	private		Long						myStartStampMsec;
	private		Double						myEndOffsetSec;

	private		Double						myFrameLengthSec;
	private		Integer						myPlanFrameCount;

	// Support for disturbance rejection
	private		JVFrame						myFirstVelocityJumpFrame;

	// For debugging
	public		GazeDimension				myGazeDimension;
	public		Integer						myPlanIterationNumber = 0;

	public SimpleGazeDimensionPlan(GazeDimension gd, NumberFactory<RN> numberFactory, double frameLengthSec) {
		myNumberFactory = numberFactory;
		myCurveMatrix = new RampingFramedCACM<RN>(myNumberFactory);
		myFrameLengthSec = frameLengthSec;
		myTrajList = new SummableWJTrajectoryList<SimpleGazeJointTrajectory>();
		myWorldJointList = new SummableWorldJointList<GazeJoint>();
		myGazeDimension = gd;
	}

	protected SimpleGazeJointTrajectory makeTrajectory(GazeJoint gj) {
		RampingFramedCurveSeq seq =  myCurveMatrix.makeSequence(gj.getJoint().getJointName());
		SimpleGazeJointTrajectory t = new SimpleGazeJointTrajectory(gj, seq, myFrameLengthSec, myNumberFactory);
		myTrajList.addTrajectory(t);
		return t;
	}
	public SimpleGazeJointTrajectory getTrajectoryForJoint(Joint j) {
		return myTrajList.getTrajectoryForJoint(j);
	}
	public SummableWorldJointList getSummableWorldJointList() {
		return myWorldJointList;
	}
	public void syncJointState(GazeJointStateSnap gjss) {
		Joint j = gjss.getJoint();
		GazeJoint gj = gjss.getGazeJoint();
		SimpleGazeJointTrajectory t = getTrajectoryForJoint(j);
		if (t == null) {
			t = makeTrajectory(gj);
		}
		t.setInititalJointStateSnap(gjss);
		if (!myWorldJointList.contains(gj)) {
			myWorldJointList.addWorldJoint(gj);
		}
	}
	private void setAllCurveSeqParams(double goalDeltaDeg) {
		for (SimpleGazeJointTrajectory t : myTrajList.getTrajectories()) {
			t.setCurveSeqParams(goalDeltaDeg);
		}
	}

	public WorldGoalPosition getTruncatedWorldGoalPosition(WorldGoalPosition wgp) {
		return myWorldJointList.getTruncatedGoalPosition(wgp);
	}
	public double getTotalInitialWorldPositionDeg() {
		return myTrajList.getTotalInitialWorldPositionDeg();
	}
	public void markStarted() {
		myStartStampMsec = TimeUtils.currentTimeMillis();
		// Hack for debugging.
		myPlanIterationNumber++;
	}
	private double getCurrentTimeOffsetSec() {
		long currentTime = TimeUtils.currentTimeMillis();
		long offsetMsec = currentTime - myStartStampMsec;
		double offsetSec = offsetMsec / 1000.0;
		return offsetSec;
	}
	private double getEndTimeOffsetSec() {
		return myEndOffsetSec;
	}
	public Integer getPlanFrameCount() {
		return myPlanFrameCount;
	}
	public boolean isCompleted() {
		double endTimeOffsetSec = getEndTimeOffsetSec();
		double currentOffsetSec = getCurrentTimeOffsetSec();
		return (currentOffsetSec > endTimeOffsetSec);
	}
	private String iterDebugHeader(int frameCount) {
		return "SGDP[dim=" + myGazeDimension
					+ ", iter=" + myPlanIterationNumber
					+ ", frames=" + frameCount + "]";
	}
	public void calculateParams(WorldGoalPosition goalWorldPos) {
		double goalDeltaWorldDeg = goalWorldPos.deltaDegrees;
		
		int maxFrames = 100;
		int warningModulusFrames = 50;
		// How long must the plan be before we treat lack of incremental
		// improvement as a termination criteria?
		// (Hint:  Sensible value depends on braking power!)
		int yieldPrudenceFrames = 10;
		double yieldPrudenceThresh = 0.001;
		double chosenTruncSizeWarningThresh = 5.0;
		double trialTruncSizeWarningThresh = 20.0;
		double chosenNegYieldWarnThresh = -1.0;
		double trialNegYieldWarnThresh = -10.0;

		// This calc is an expensive operation:
		double maxYield = calculateMaxYieldParamMatrix(goalDeltaWorldDeg, maxFrames,
					yieldPrudenceFrames, yieldPrudenceThresh, warningModulusFrames,
					trialTruncSizeWarningThresh, trialNegYieldWarnThresh);
		int planFrames = getPlanFrameCount();
		String iterDbgHeader = "CHOSEN-" + iterDebugHeader(planFrames);

		double yieldReductionFactor = Math.abs(goalDeltaWorldDeg) / maxYield;
		if (yieldReductionFactor < 0.0 || yieldReductionFactor > 1.02) {
			if (planFrames >= yieldPrudenceFrames) {
				// Note:  In practice, we are currently avoiding this condition by specifying
				// a huge allowed braking rate (10000.0 DPSPS)		
				theLogger.warn(iterDbgHeader + " exceeds yieldPrudenceFrames=" + yieldPrudenceFrames
						+ ", so forcing excessive yieldReductionFactor from " + yieldReductionFactor
						+ " to 1.0 (instead of throwing error)");
				yieldReductionFactor = 1.0;
			}
			else {
				// This does not fit when calc was aborted due to lack of yield progress.
				throw new RuntimeException(iterDbgHeader + " is acceptable at " + planFrames + ", but yieldReductionFactor is outside unit interval: " + yieldReductionFactor);
			}
		}
		// YRF is often significantly below 1.0, indicating that we will overshoot
		// until it is really applied.
		// theLogger.info("yieldReductionFactor=" + yieldReductionFactor);
		for (SimpleGazeJointTrajectory t : myTrajList.getTrajectories()) {
			// Somewhat expensive
			double currentYield = t.actualYieldForCurrentParamsUsingBoundaries(iterDbgHeader, chosenTruncSizeWarningThresh, -3.0);
			double targetYield = currentYield * yieldReductionFactor;
			// TODO:  This does not yet trigger any actual adjustment in the
			// accelerations of the curves!
			t.setTargetDegreeYield(targetYield);
		}
//		theLogger.info("CurveMatrix targeting pos-delta: " + goalDeltaDeg
//				+ "in " + myPlanFrameCount + " frames is\n"
//				+ myCurveMatrix.dumpSamples(myPlanFrameCount + 1, myPlanFrameCount * myFrameLengthSec));

	}

	/*
	 *  This method is the main cost center of the rampy gaze calc.
	 */
	private double calculateMaxYieldParamMatrix(double goalDeltaDeg, int maxFrames,
				int yieldPrudenceFrames, double yieldPrudenceThresh,
				int planTediumWarningModulusFrames, double truncWarningThresh,
				double negYieldWarnThresh) {
		setAllCurveSeqParams(goalDeltaDeg);
		int minFrames = myCurveMatrix.minFramesToStopAllSeqsFromInitVel();
		int numFrames = (minFrames > 0) ? minFrames : 1;
		double lastTotalMaxYield = 0.0;
		double totalMaxYield;
		while (true) {
			String trialDebugHeader = "TRIAL-" + iterDebugHeader(numFrames);
			// Determine the shortest plan that will get us to our goal, by
			// increasing the number of frames until we get sufficient yield.
			// TODO:  improve performance by increasing frames in larger jumps,
			// perhaps doing a binary search for best #frames.
			totalMaxYield = 0.0;
			for (SimpleGazeJointTrajectory t : myTrajList.getTrajectories()) {
				// Each of these next two calls is costly, because it results
				// in a number of polynomial evaluations (and hence RN multiplies,
				// and hence RN constructions) proportional to the number
				// of curves in each sequence (i.e. the number of Phases.)
				t.calcNaiveParamsForMaximumYield(numFrames);
				double seqMaxYield = t.actualYieldForCurrentParamsUsingBoundaries(trialDebugHeader, truncWarningThresh,
						negYieldWarnThresh);
				totalMaxYield += seqMaxYield;
			}
			if (totalMaxYield >= Math.abs(goalDeltaDeg)) {
				break;
			} else {
				double yieldImprovement = totalMaxYield - lastTotalMaxYield;
				lastTotalMaxYield = totalMaxYield;
				if ((numFrames >= yieldPrudenceFrames) && (yieldImprovement < yieldPrudenceThresh)) {
					// Generally this happens because of servo boundaries, especially in
					// a gaze plan where some joints are not available AND those joints are
					// currently pointing in the wrong direction.
					// TODO:  Improve goal truncation to take this last factor into account.
					theLogger.info("Plan[iter=" + myPlanIterationNumber + "]: after " + numFrames + ", our yieldImprovement is only " + yieldImprovement
								+ ", so terminating plan here, with maxYield(positive)=" + totalMaxYield
								+ " short of the goal(signed)=" + goalDeltaDeg);
					break;
				}
				numFrames++;
				if ((numFrames % planTediumWarningModulusFrames) == 0) {
					
					theLogger.warn("Plan[iter=" + myPlanIterationNumber + "]: length is now " + numFrames + " frames!  - GoalDeltaDeg is still "
								+ goalDeltaDeg + " while totalMaxYield(always positive)=" + totalMaxYield
								);// + ", plan=[" + toString() + "]");
					/*
					theLogger.info("PlanMatrix: "
							+ myCurveMatrix.dumpSamples(10, 10 * myFrameLengthSec)
							+ myCurveMatrix.dumpSamples(10, numFrames * myFrameLengthSec));
					*/
					// negativeYieldWarnFlag = true;

				} else {
					// negativeYieldWarnFlag = false;
				}
				if (numFrames >= maxFrames) {
					theLogger.warn("************ Aborting planning phase: " + trialDebugHeader);
					break;
				}
			}
		}
		myPlanFrameCount = numFrames;
		// All curves now have their "correct" durations, but accels may be too high.
		return totalMaxYield;
	}
	public JVFrame getFirstVelocityFrame() {
		double offsetSec = 0.0;  // getCurrentTimeOffsetSec();
		double targetOffsetSec = offsetSec + myFrameLengthSec;
		myFirstVelocityJumpFrame = myTrajList.getVelocityFrameForJumpFromStartToTime(targetOffsetSec);
		return myFirstVelocityJumpFrame;
	}
	public Frame getPreviousFirstVelocityFrame() {
		return myFirstVelocityJumpFrame;
	}
	public boolean isWorldGoalInRange(WorldGoalPosition wgp, boolean warnOnFailure) {
		return myTrajList.isGoalInRange(wgp, warnOnFailure);
	}
	@Override public String toString() {
		return "SimpleGazeDimPlan[iter=" + myPlanIterationNumber + ", curveMatrix=" + myCurveMatrix + "]";
	}
}
