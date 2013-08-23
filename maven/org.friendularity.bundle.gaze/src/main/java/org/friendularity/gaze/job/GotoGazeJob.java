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

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.cogchar.animoid.calc.curvematrix.ConstAccelCurveMatrix;
import org.friendularity.gaze.estimate.GazeJointStateSnap;
import org.friendularity.gaze.plan.SimpleGazeDimensionPlan;
import org.friendularity.gaze.plan.SimpleGazePlanManager;
import org.cogchar.api.animoid.config.bonus.AnimoidConfig;
import org.friendularity.gaze.api.GazeDimension;
import org.friendularity.gaze.api.GazeJointStrategy;
import org.cogchar.api.animoid.protocol.JVFrame;
import org.cogchar.api.animoid.protocol.JointVelocityAROMPS;
import org.cogchar.api.animoid.world.WorldGoalPosition;
import org.appdapter.bind.math.jscience.number.Float64Funcs;
import org.appdapter.bind.math.jscience.number.NumberFactory;
import org.friendularity.gaze.api.AnimoidGazeConfig;
import org.friendularity.gaze.plan.SimpleGazeDimensionPlan;
import org.friendularity.gaze.plan.SimpleGazePlanManager;
import org.friendularity.gaze.api.GazeDimension;
import org.friendularity.gaze.api.GazeJointStrategy;
import org.jscience.mathematics.number.Float64;
import org.jscience.mathematics.number.Real;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 * @author Stu B. <www.texpedient.com>
 */
public class GotoGazeJob extends GazeJob {

	private static Logger	theLogger = LoggerFactory.getLogger(GotoGazeJob.class.getName());

	private int		myFrameCount = 0;
    private Map<GazeDimension, Boolean> mySlackCache;

	private SimpleGazePlanManager<Float64>		myRampyGazePlanManager;

	public GotoGazeJob(AnimoidGazeConfig aconf) {
		super(aconf);
        mySlackCache = new HashMap<GazeDimension, Boolean>();
	}
	public SimpleGazePlanManager<Float64> getRampyGazePlanManager() {
		if (myRampyGazePlanManager == null) {
			NumberFactory<Float64> numFact = Float64Funcs.getNumberFactory();
			double nominalFrameDurSec = getTimeKeeper().getNominalSecPerFrame();
			myRampyGazePlanManager = new SimpleGazePlanManager<Float64>(numFact, nominalFrameDurSec);
		}
		return myRampyGazePlanManager;
	}
	@Override public String getTypeString() {
		return "GotoGazeJob";
	}

	@Override protected JVFrame computeDimensionVelFrame(GazeDimension d, List<GazeJointStateSnap> stats,
			double goalDeg, double goalDeltaDeg, double previousTotalVelDPS) {
		JVFrame result = null;
		try {
			if (d == GazeDimension.HORIZONTAL) {
				myFrameCount++;
			}
			int refreshPeriodFrames = myGazeStrategy.getRefreshPeriodFrames();
			if ((myFrameCount % refreshPeriodFrames) != 0) {
				return null;
			}
			switch(myGazeStrategy.getMotionStyle()) {
				case JUMPY:
					result = makeJumpyDimVelFrame(d, stats, goalDeg, goalDeltaDeg,	previousTotalVelDPS);
				break;
				case RAMPY:
					result = makeRampyDimVelFrame(d, stats, goalDeg, goalDeltaDeg, previousTotalVelDPS);
				break;
				case NONE:
					result = null;
				break;
				default:
					throw new RuntimeException("Unknown motionStyle: " + myGazeStrategy.getMotionStyle());
			}
			// result = computePlannedDimVelFrame(d, stats, goalDeg, 3, 0.0, 4.0, 0.1);
			// theLogger.info("OptimizedVelFrame: " + result);
		} catch (Throwable t) {
			theLogger.error("problem optimizing gotoGaze velFrame", t);
		}
		
		return result;
	}

	protected JVFrame makeJumpyDimVelFrame(GazeDimension d, List<GazeJointStateSnap> stats,
			double goalDeg, double goalDeltaDeg, double previousTotalVelDPS) {
		JVFrame velFrame = new JVFrame();

		double nominalFrameDurSec = getTimeKeeper().getNominalSecPerFrame();
		double goalSec = nominalFrameDurSec;
		if (isWithinSlack(d, goalDeltaDeg)) {
			return null;
		}
		Double slackDeg = getSlackDegrees(d);
		if (slackDeg == null) {
			slackDeg = 0.0;
		}
		double scaledDeltaDeg = goalDeltaDeg * myGazeStrategy.distanceJumpRatio 
					+ myGazeStrategy.flatJumpSize * Math.signum(goalDeltaDeg);
		double brakeTerm = 1.0 - myGazeStrategy.brakeSlope * slackDeg / Math.abs(goalDeltaDeg);
		double brakeCoeff = Math.pow(brakeTerm, myGazeStrategy.brakePower);
		double idealTotalVelDPS = brakeCoeff * scaledDeltaDeg / goalSec;
		// double idealDeltaVelImpulse = idealTotalVelDPS - totalVelDPS;
		// double idealTotalAccel = idealDeltaVelImpulse / 15.0; // Umm, 2 sec total impulse delivery.  Yeah!
		/*theLogger.info("goalDeltaDeg=" + goalDeltaDeg + ", totalVelDPS=" + totalVelDPS + ", goalSec=" + goalSec 
				+ "idealTotalVelDPS=" + idealTotalVelDPS + ", idealDeltaVelImpulse=" + idealDeltaVelImpulse 
				+ ", idealTotalAccel=" + idealTotalAccel 
				+ ", tstamp=" + TimeUtils.currentTimeMillis());
		*/
		int numStats = stats.size();
		for (GazeJointStateSnap stat : stats) {
			// theLogger.info(stat.toString());
			// double weight = 1.0 / numStats; // stat.myGazeJointStrategy.getNormalizedWeight();
			GazeJointStrategy gjs = stat.getGazeJointStrategy();
			if (gjs == null) {
				throw new RuntimeException("Wacky: GazeJointStateSnap has null strategy: " + gjs);
			}
			// double weight = 1.0 / gjs.getWorkFuelCostWeight();
			double weight = gjs.flatMotionWeight;
			if (!stat.getGazeJoint().isEgocentricDirectionSensePositive()) {
				weight = -1.0 * weight;
			}
			double velDPS = idealTotalVelDPS * weight;
			double velROM = velDPS / stat.getTotalRomDegrees();
			theLogger.trace(stat.getShortDescriptiveName() + " vel-ROM-ps=" + velROM
					+ " vel-Deg-ps=" + velDPS);
			JointVelocityAROMPS jvel = new JointVelocityAROMPS(stat.getJoint(), velROM);
			velFrame.addPosition(jvel);
/*  Old experimental acceleration calc
			double accel = idealTotalAccel * weight;
			// theLogger.info("accel for " + stat.myJoint + " computed as: " + accel);
			JointPosition accelJP = new JointPosition(stat.myJoint);
			accelJP.setCoordinateFloat(JointPosition.CoordinateType.FLOAT_ACC_RANGE_OF_MOTION_PSPS, accel);
			accelFrame.addPosition(accelJP);
 */
		}
		return velFrame;
	}
	protected JVFrame makeRampyDimVelFrame(GazeDimension d, List<GazeJointStateSnap> stats,
			double goalDeg, double goalDeltaDeg, double previousTotalVelDPS) {
		JVFrame velFrame = null;
		SimpleGazeDimensionPlan<Float64> sgdp = getRampyGazePlanManager().getPlanForStrategyAndDimension(myGazeStrategy.getName(), d);
		for (GazeJointStateSnap stat: stats) {
			sgdp.syncJointState(stat);
		}
		WorldGoalPosition wgp = new WorldGoalPosition(goalDeg, goalDeltaDeg);
		WorldGoalPosition adjustedWGP = sgdp.getTruncatedWorldGoalPosition(wgp);
		// theLogger.info("GoalPos for dim=" + d + " was[" + wgp + "], truncated to[" + adjustedWGP + "]");
		if (isWithinSlack(d, adjustedWGP.deltaDegrees)) {
			return null;
		}
		sgdp.calculateParams(adjustedWGP);
		sgdp.markStarted();
		velFrame = sgdp.getFirstVelocityFrame();
		return velFrame;
	}

	private Map<GazeDimension, ConstAccelCurveMatrix<Real>> mySimpleDimPlans =
				new HashMap<GazeDimension, ConstAccelCurveMatrix<Real>>();

	public boolean isWithinSlack(GazeDimension d, double goalDeltaDeg) {
		Double slackDeg = getSlackDegrees(d);
		if (slackDeg == null) {
			slackDeg = 0.0;
		}
        Boolean withinSlack = Math.abs(goalDeltaDeg) <= slackDeg;
       //  mySlackCache.put(d, withinSlack);
		return withinSlack;
	}
/*
    public Boolean isWithinSlackCache(GazeDimension d){
        return mySlackCache.get(d);
    }
*/
}
