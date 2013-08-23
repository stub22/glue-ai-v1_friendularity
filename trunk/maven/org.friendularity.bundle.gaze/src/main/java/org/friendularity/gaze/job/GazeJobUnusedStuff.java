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

package org.friendularity.gaze.job;

import java.util.List;

import org.friendularity.gaze.estimate.GazeJointStateSnap;

import org.friendularity.gaze.api.AnimoidGazeConfig;

import org.cogchar.api.animoid.protocol.Frame;
import org.cogchar.api.animoid.protocol.JointPosition;
import org.cogchar.api.animoid.protocol.JointStateCoordinateType;
import org.cogchar.platform.util.TimeUtils;
import org.friendularity.gaze.api.GazeDimension;
import org.friendularity.gaze.plan.GazeDimensionMotionPlan;
import org.friendularity.gaze.plan.OptimizingGDMP;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 * @author Stu B. <www.texpedient.com>
 */
public class GazeJobUnusedStuff extends GotoGazeJob {
	private static Logger	theLogger = LoggerFactory.getLogger(GazeJobUnusedStuff.class.getName());

	public GazeJobUnusedStuff(AnimoidGazeConfig aconf) {
		super(aconf);
	}
	// @Override
	protected GazeDimensionMotionPlan makeGDMP(List<GazeJointStateSnap> stats, Integer numIntervals,
				Double planStartSec, Double planDurSec, Double frameLengthSec) {

		GazeDimensionMotionPlan plan =  new OptimizingGDMP(numIntervals, planStartSec, planDurSec, frameLengthSec);
		for (GazeJointStateSnap stat: stats) {
			plan.addJoint(stat);
		}
		plan.completeInitAfterJointsAdded();
		return plan;
	}
	@Override protected void updateGDMP(GazeDimensionMotionPlan gdmp, List<GazeJointStateSnap> stats, double goalDeg)
				throws Throwable {
		super.updateGDMP(gdmp, stats, goalDeg);
		if (stats.size() == 0) {
			theLogger.warn("Ignoring request to optimize velFrame with empty stats list");
		}

		long optiStartStamp =  TimeUtils.currentTimeMillis();
		// .info("Starting optimization at: " + optiStartStamp + " in pursuit of goalDeg: " + goalDeg);
		// TODO:  Pass in a polynomial describing a "nice" gaze trajectory here.
		((OptimizingGDMP) gdmp).optimize(goalDeg);
		long optiEndStamp = TimeUtils.currentTimeMillis();
		double optiElapsedSec = (optiEndStamp - optiStartStamp) / 1000.0;
		// theLogger.info("Finished optimization at: " + optiEndStamp + ", optimElapsed=" + optiElapsedSec) ;
		// theLogger.info("OptimizedPlan to pursue " + goalDeg +  " is: " + gdmp);
	}
	protected GazeDimensionMotionPlan getDimensionPlan(GazeDimension d,  List<GazeJointStateSnap> stats,
			Integer numIntervals, Double planStartSec, Double planDurSec, Double frameLengthSec) {
		GazeDimensionMotionPlan plan = null;
		if (d == GazeDimension.HORIZONTAL) {
			plan = myHorizGDMP;
		} else {
			plan = myVertGDMP;
		}
		if (plan == null) {
			plan =  makeGDMP(stats, numIntervals, planStartSec, planDurSec, frameLengthSec);
		}
		if (d == GazeDimension.HORIZONTAL) {
			myHorizGDMP = plan;
		} else {
			myVertGDMP = plan;
		}
		return plan;
	}
	protected Frame computePlannedDimVelFrame(GazeDimension d, List<GazeJointStateSnap> stats, double goalDeg,
				Integer numIntervals, Double planStartSec, Double planDurSec, Double frameLengthSec) throws Throwable {

		long planStartStamp =  TimeUtils.currentTimeMillis();
		// theLogger.info("Starting plan construction at: " + planStartStamp + " in pursuit of goalDeg: " + goalDeg);

		GazeDimensionMotionPlan gdmp = getDimensionPlan(d,  stats,
				 numIntervals,  planStartSec,  planDurSec,  frameLengthSec);
		updateGDMP(gdmp, stats, goalDeg);
		Frame velFrame = null;

		velFrame = new Frame();
		for (GazeJointStateSnap stat: stats) {
			double jointRomSense = stat.getGazeJoint().isEgocentricDirectionSensePositive()?1.0:-1.0;
			// These acceleration
			Double accelDegPSPS = gdmp.getInitialPlannedAccelDegPSPSForJoint(stat.getGazeJoint());
			Double accelRomPSPS = jointRomSense * accelDegPSPS / stat.getTotalRomDegrees();
			double velocitySampleTimeOffset = frameLengthSec; // "end of this frame"
			double velDegPS = gdmp.getPlannedVelDegPSForJointAtTimeOffset(stat.getGazeJoint(), velocitySampleTimeOffset); //
			// These two ways of computing velRomPS are the same if timeOffset is frameLengthSec,
			// and if the plan takes myVelRomPerSec into account as we expect.
			double velRomPS = jointRomSense * velDegPS /  stat.getTotalRomDegrees();
			// double velRomPS = stat.myVelRomPerSec + accelRomPSPS * frameLengthSec;
			JointPosition velJP = new JointPosition(stat.getJoint());
			velJP.setCoordinateFloat(JointStateCoordinateType.FLOAT_VEL_RANGE_OF_MOTION_PER_SEC, velRomPS);
			velFrame.addPosition(velJP);
			theLogger.trace(stat.getJoint().getJointName() + " accelDeg=" + accelDegPSPS
						+ ", accelROM=" + accelRomPSPS
						+ ", velROM=" + velRomPS);
		}
		long endRenderStamp = TimeUtils.currentTimeMillis();
		double totalPlanElapsedSec = (endRenderStamp - planStartStamp)/1000.0;
		// theLogger.info("Finished rendering dimension frame at: " + endRenderStamp
		//			+ ", totalDimPlanElapsedSec=" + totalPlanElapsedSec);
		return velFrame;
	}

	protected Frame computeSimplyPlannedDimVelFrame(GazeDimension d, List<GazeJointStateSnap> stats, double goalDeg,
				Integer numIntervals, Double planStartSec, Double planDurSec, Double frameLengthSec) throws Throwable {

		long planStartStamp =  TimeUtils.currentTimeMillis();
		// theLogger.info("Starting plan construction at: " + planStartStamp + " in pursuit of goalDeg: " + goalDeg);

		GazeDimensionMotionPlan gdmp = getDimensionPlan(d,  stats,
				 numIntervals,  planStartSec,  planDurSec,  frameLengthSec);
		updateGDMP(gdmp, stats, goalDeg);
		Frame velFrame = null;

		velFrame = new Frame();
		for (GazeJointStateSnap stat: stats) {
			double jointRomSense = stat.getGazeJoint().isEgocentricDirectionSensePositive()?1.0:-1.0;
			// These acceleration
			Double accelDegPSPS = gdmp.getInitialPlannedAccelDegPSPSForJoint(stat.getGazeJoint());
			Double accelRomPSPS = jointRomSense * accelDegPSPS / stat.getTotalRomDegrees();
			double velocitySampleTimeOffset = frameLengthSec; // "end of this frame"
			double velDegPS = gdmp.getPlannedVelDegPSForJointAtTimeOffset(stat.getGazeJoint(), velocitySampleTimeOffset); //
			// These two ways of computing velRomPS are the same if timeOffset is frameLengthSec,
			// and if the plan takes myVelRomPerSec into account as we expect.
			double velRomPS = jointRomSense * velDegPS /  stat.getTotalRomDegrees();
			// double velRomPS = stat.myVelRomPerSec + accelRomPSPS * frameLengthSec;
			JointPosition velJP = new JointPosition(stat.getJoint());
			velJP.setCoordinateFloat(JointStateCoordinateType.FLOAT_VEL_RANGE_OF_MOTION_PER_SEC, velRomPS);
			velFrame.addPosition(velJP);
			theLogger.trace(stat.getJoint().getJointName() + " accelDeg=" + accelDegPSPS
						+ ", accelROM=" + accelRomPSPS
						+ ", velROM=" + velRomPS);
		}
		long endRenderStamp = TimeUtils.currentTimeMillis();
		double totalPlanElapsedSec = (endRenderStamp - planStartStamp)/1000.0;
		// theLogger.info("Finished rendering dimension frame at: " + endRenderStamp
		//			+ ", totalDimPlanElapsedSec=" + totalPlanElapsedSec);
		return velFrame;
	}

}
