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

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Set;

import org.friendularity.gaze.estimate.GazeDirectionComputer;
import org.friendularity.gaze.estimate.GazeJointStateSnap;

import org.cogchar.animoid.job.MotionJob;

import org.friendularity.gaze.api.GazeDimension;
import org.friendularity.gaze.api.GazeJoint;
import org.friendularity.gaze.api.GazeJointStrategy;
import org.friendularity.gaze.api.StereoGazeConfig;
import org.friendularity.gaze.util.GazeStrategyCue;

import org.cogchar.api.animoid.protocol.EgocentricDirection;
import org.cogchar.api.animoid.protocol.Frame;
import org.cogchar.api.animoid.protocol.JVFrame;
import org.cogchar.api.animoid.protocol.Joint;
import org.cogchar.api.animoid.protocol.JointPosition;
import org.cogchar.api.animoid.protocol.JointStateCoordinateType;
import org.cogchar.api.animoid.protocol.JointVelocityAROMPS;
import org.cogchar.api.animoid.protocol.SmallAngle;
import org.cogchar.api.animoid.world.WorldJoint;
import org.cogchar.zzz.platform.stub.JobStub.Status;
import org.friendularity.gaze.api.AnimoidGazeConfig;
import org.friendularity.gaze.plan.GazeDimensionMotionPlan;
import org.friendularity.sight.track.IGazeTarget;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Stu B. <www.texpedient.com>
 */
public abstract class  GazeJob extends MotionJob {
	private static Logger	theLogger = LoggerFactory.getLogger(GazeJob.class.getName());
	protected			GazeStrategyCue				myGazeStrategy;
	
	protected		IGazeTarget					myGazeTarget;
	
	protected		GazeDirectionComputer		myGazeDirectionComputer;
	
	protected		Set<Joint>					myCachedCautionJoints;

	protected		GazeDimensionMotionPlan		myHorizGDMP, myVertGDMP;
	// My output vel frame, before it was blended and truncated by the blender.
	protected		JVFrame						myLastNaiveContribVelFrame;
	protected		EgocentricDirection			myLastGoalDir, myLastGoalDeltaDir;
	public			Double						myLastHorizYieldDPS, myLastVertYieldDPS, myLastNormRDCF;

	public GazeJob(AnimoidGazeConfig aconf) {
		super(aconf);
	}
	public AnimoidGazeConfig getAnimoidGazeConfig() {
		return (AnimoidGazeConfig) getAnimoidConfig();
	}
	abstract protected JVFrame computeDimensionVelFrame(GazeDimension d, List<GazeJointStateSnap> stats,
				double goalDeg, double goalDeltaDeg,  double previousTotalVelDPS);

	// protected abstract GazeDimensionMotionPlan makeGDMP(List<GazeJointStateSnap> stats,
	//			Integer numIntervals, Double planStartSec, Double planDurSec, Double frameLengthSec);

	protected void setGazeStrategy (GazeStrategyCue gsc) {
		// Protected - because this call should always come from AttentionJob
		theLogger.info("Setting strategy to: " + gsc.getName());
		myGazeStrategy = gsc;
		myHorizGDMP = null;
		myVertGDMP = null;
		// myLastNaiveContribVelFrame = null;
		// Not necessary, because caution-joints are not gaze-strategy specific.
		// myCachedCautionJoints = null;
        markUpdatedNow();
	}
	public void setGazeDirectionComputer (GazeDirectionComputer gdc) {
		myGazeDirectionComputer = gdc;
	}
	protected void setGazeTarget (IGazeTarget gt) {
		// Protected - because this call should always come from AttentionJob
		theLogger.info("Setting GazeJob.gazeTarget to: " + gt);
		myGazeTarget = gt;
		// myLastNaiveContribVelFrame = null;
	}
	protected IGazeTarget getGazeTarget() {
		return myGazeTarget;
	}
	protected GazeStrategyCue getGazeStrategy() {
		return myGazeStrategy;
	}
    public String getStrategyName(){
        if(myGazeStrategy == null){
            return null;
        }
        return myGazeStrategy.getName();
    }
	@Override public String getContentSummaryString() {
        if(myGazeStrategy == null){
            return "GazeJobContents";
        }
		return "Strategy=" + myGazeStrategy.getName();
	}

	@Override public Collection<Joint> getCautionJoints() {
		// We always return ALL gaze joints known to the animoidConfig
		// (not just the ones used by the current strategy, and regardless of
		// our enabled/disabled state).
        if (myCachedCautionJoints == null) {
            myCachedCautionJoints = getAnimoidGazeConfig().getAllGazeBoundJoints();
        }
        return myCachedCautionJoints;
	}

	@Override public  JVFrame  contributeVelFrame(Frame currentPosEstimateAbsROM, JVFrame prevVelRomFrame, Set<Joint> cautionJoints) {
		JVFrame contribVelFrame = computeVelFrame(currentPosEstimateAbsROM, prevVelRomFrame);
		myLastNaiveContribVelFrame = contribVelFrame;
		return contribVelFrame;
	}

	protected synchronized JVFrame computeVelFrame(Frame currentPosEstimateAbsROM, JVFrame prevVelRomFrame) {
		if(this.getStatus() != Status.RUNNING) {
			theLogger.trace("Gaze attention is disabled, so nothing to do");
			return null;
		}
		JVFrame stereoVisionVelFrame = computeStereoGazePartnerVelFrame(currentPosEstimateAbsROM);
		JVFrame nominalVelFrame = computeNominalVelFrameForTargetUsingCurrentStrategy(currentPosEstimateAbsROM, prevVelRomFrame);
		if (nominalVelFrame == null) {
			return null;
		}

		JVFrame sumVelFrame = JVFrame.sumJVFrames(nominalVelFrame, stereoVisionVelFrame);
		// theLogger.finer("Returning Gaze velFrame: " + sumVelFrame);
		JVFrame contribVelFrame = sumVelFrame;
		if ((myLastNaiveContribVelFrame != null) && (prevVelRomFrame != null)) {
			// PROBLEM : Truncation at extremes of motion is herein treated as a 
			// "disturbance", which leads to a cumulative velocity build up
			// (i.e. integrator windup) as we try to push against
			// that measured disturbance.

			// SOLUTION : Disturbance rejection is currently disabled.

			// TODO:  We should only apply disturbance rejection when the
			// planned deltas are relatively large.    This will allow gestures
			// with small gaze disturbance to proceed unmolested.
			JVFrame disturbanceCorrection = JVFrame.weightedSumJVFrameCommonJoints(myLastNaiveContribVelFrame, 1.0,
					prevVelRomFrame, -1.0);
			JVFrame relevantDCFrame = disturbanceCorrection.subJVFrame(sumVelFrame.getUsedJointSet(), true);
			double normRDCF = relevantDCFrame.computeNorm(1.0, JointStateCoordinateType.FLOAT_VEL_RANGE_OF_MOTION_PER_SEC);
			myLastNormRDCF = normRDCF;

			double DISTURBANCE_REJCECTOR_ATTENUATION = 0.5;
			// theLogger.info("relevantDCFrame: " + relevantDCFrame);
			// contribVelFrame = Frame.sumCompatibleFrames(sumVelFrame, relevantDCFrame);
			contribVelFrame = sumVelFrame;
		}
		return contribVelFrame;
	}
	protected synchronized JVFrame computeNominalVelFrameForTargetUsingCurrentStrategy(Frame currentPosEstimateAbsROM, Frame prevVelRomFrame) {
		EgocentricDirection goalAngles = getTargetDirectionEstimate();
		EgocentricDirection moveAngles = getTargetOffsetEstimate(currentPosEstimateAbsROM);
		myLastGoalDir = goalAngles;
		myLastGoalDeltaDir = moveAngles;

		if (moveAngles == null) {
			theLogger.debug("No target angles, so no move required");
			return null;
		}
		if (myGazeStrategy  == null) {
			theLogger.debug("No gaze plan, so no move calculated");
			return null;
		}

		List<GazeJointStrategy> horizLinks = myGazeStrategy.getHorizLinks();
		List<GazeJointStrategy> vertLinks = myGazeStrategy.getVertLinks();
		JVFrame nominalVelFrame = computeNominalGazeVelFrameFromLinksAndAngles
			(horizLinks, vertLinks, goalAngles, moveAngles, currentPosEstimateAbsROM, prevVelRomFrame);
		return nominalVelFrame;
	}

	protected synchronized JVFrame computeNominalGazeVelFrameFromLinksAndAngles(
				List<GazeJointStrategy> horizLinks,
				List<GazeJointStrategy> vertLinks,
				EgocentricDirection goalAngles,
				EgocentricDirection moveAngles,
				Frame currentPosEstimateAbsROM,
				Frame prevVelRomFrame) {

		SmallAngle azimuthGoal = goalAngles.getAzimuth();
		SmallAngle elevGoal = goalAngles.getElevation();
		SmallAngle azimuthDelta = moveAngles.getAzimuth();
		SmallAngle elevDelta = moveAngles.getElevation();
		// myLastNaiveContribVelFrame = null;

		JVFrame horizVelFrame = computeDimensionVelFrame(GazeDimension.HORIZONTAL, horizLinks, azimuthGoal, azimuthDelta, currentPosEstimateAbsROM, prevVelRomFrame);
		JVFrame vertVelFrame = computeDimensionVelFrame(GazeDimension.VERTICAL, vertLinks, elevGoal, elevDelta, currentPosEstimateAbsROM, prevVelRomFrame);
		double horizNominalYieldDPS = computeTotalVelDPS_ForDimVelFrame(horizVelFrame, horizLinks);
		double vertNominalYieldDPS = computeTotalVelDPS_ForDimVelFrame(vertVelFrame, vertLinks);
		myLastHorizYieldDPS = horizNominalYieldDPS;
		myLastVertYieldDPS = vertNominalYieldDPS;
		JVFrame subtotalVelFrame = JVFrame.sumJVFrames(horizVelFrame, vertVelFrame);
		return subtotalVelFrame;
	}

	public JVFrame computeDimensionVelFrame(
				GazeDimension d, List<GazeJointStrategy> links,
				SmallAngle goal, SmallAngle delta,
				Frame prevPosAbsRomFrame, Frame prevVelRomFrame) {

		List<GazeJointStateSnap> stats = new ArrayList<GazeJointStateSnap>();
		double goalDeg = goal.getDegrees();
		double goalDeltaDeg = delta.getDegrees();

		double previousTotalVelDPS = 0.0;
		for (GazeJointStrategy gjl: links) {
			GazeJointStateSnap stat = new GazeJointStateSnap(gjl, prevPosAbsRomFrame, prevVelRomFrame);
			stats.add(stat);
			previousTotalVelDPS += stat.getWorldVelDegPerSec(); // getInternalVelDegPerSec();
		}
		return computeDimensionVelFrame(d, stats, goalDeg, goalDeltaDeg, previousTotalVelDPS);
	}

	public double computeTotalVelDPS_ForDimVelFrame(Frame<JointVelocityAROMPS> f, List<GazeJointStrategy> links) {
		double totalWorldDPS = 0.0;
		if (f!= null) {
			for (GazeJointStrategy gjs : links) {
				WorldJoint wj = gjs.getGazeJoint();
				Joint j = wj.getJoint();
				JointVelocityAROMPS jv = f.getJointPositionForJoint(j);
				double worldDPS = wj.getWorldAngleSpeedDegPS_forVelAROMPS(jv);
				totalWorldDPS += worldDPS;
			}
		}
		return totalWorldDPS;
	}
	public JVFrame computeStereoGazePartnerVelFrame(Frame prevPosAbsRomFrame) {
		JVFrame velFrame = new JVFrame();

		StereoGazeConfig sgc = getAnimoidGazeConfig().getStereoGazeConfig();
		double secPerFrame = getAnimoidGazeConfig().getSecondsPerFrame();
		if (sgc != null) {
			double defVergDeg = sgc.defaultVergenceDegrees;
			double goalVergDeg = defVergDeg;
			// if (myGazeTarget != null) {
			//	goalVergDeg = myGazeTarget.getVergenceAngle(sgc.defaultFaceWidthPixels, sgc.vergenceSlope);
			// }

			// We want eyes to be this much closer than they are at their default positions.
			double vergDisplaceDeg = goalVergDeg - defVergDeg;

			GazeJoint cameraGJ = sgc.getCameraGazeJoint();
			WorldJoint partnerWJ = sgc.getPartnerWorldJoint();
			JointPosition cameraPos = prevPosAbsRomFrame.getJointPositionForJoint(cameraGJ.getJoint());
			JointPosition partnerPos = prevPosAbsRomFrame.getJointPositionForJoint(partnerWJ.getJoint());
			double cameraCurrentWorldDeg = cameraGJ.getWorldAngleDegForROMJP(cameraPos);
			double partnerCurrentWorldDeg = partnerWJ.getWorldAngleDegForROMJP(partnerPos);

			double vergenceSignMult = sgc.getVergencePartnerWorldAngleMultiplier();
			// This calc ignores the fact that camera was already verged on
			// previous frame.
			// double cameraGoalWorldDeg = cameraCurrentWorldDeg - vergenceSignMult * (vergDisplaceDeg*0.5);
			// double partnerGoalWorldDeg = cameraCurrentWorldDeg + vergenceSignMult * (vergDisplaceDeg*0.5);

			// double cameraGoalWorldDeg = cameraCurrentWorldDeg - vergenceSignMult * (vergDisplaceDeg*0.5);
			double partnerGoalWorldDeg = cameraCurrentWorldDeg + vergenceSignMult * (vergDisplaceDeg*0.5);

				
			// theLogger.info("cameraWorldDeg=" + cameraWorldDeg
			// 			+ ", partnerCurrWorldDeg=" + partnerCurrentWorldDeg
			//			+ ", partnerGoalWorldDeg=" + partnerGoalWorldDeg);

			// For starters, let's jump this dang eye in one frame.
			JointVelocityAROMPS pjvel = partnerWJ.computeVelForJumpToTruncWorldDeg(partnerCurrentWorldDeg, partnerGoalWorldDeg, secPerFrame);
			// JointVelocityAROMPS cjvel = cameraGJ.computeVelForJumpToTruncWorldDeg(cameraCurrentWorldDeg, cameraGoalWorldDeg, secPerFrame);
			// theLogger.info("Stereo Partner velocity=" + jvel);
			velFrame.addPosition(pjvel);
			// velFrame.addPosition(cjvel);
		}
		 
		return velFrame;
	}
	protected void updateGDMP(GazeDimensionMotionPlan gdmp, List<GazeJointStateSnap> stats, double goalDeg)
				throws Throwable {
		// updateGDMP method is currently (2010-2-1) used only by unused methods
		// in GazeJobUnusedStuff.
		for (GazeJointStateSnap stat: stats) {
			gdmp.updateJointKnownState(stat);
		}
	}
	public EgocentricDirection getTargetDirectionEstimate() {
		EgocentricDirection targetDirection = null;
		if (myGazeTarget != null) {
			targetDirection = myGazeTarget.getEstimatedDirection();
		}
		return targetDirection;
	}
	public EgocentricDirection getCurrentDirectionEstimate(Frame currentPos) {
		if (myGazeDirectionComputer != null) {
			return myGazeDirectionComputer.computeGazeCenterDirection(currentPos);
		} else {
			return null;
		}
	}
	public EgocentricDirection getTargetOffsetEstimate(Frame currentPos) {
		EgocentricDirection targetOffset = null;
		EgocentricDirection targetDirection = getTargetDirectionEstimate();
		EgocentricDirection currentDirection = getCurrentDirectionEstimate(currentPos);
		if ((targetDirection != null ) && (currentDirection != null)) {
			targetOffset = targetDirection.subtract(currentDirection);
		}
		return targetOffset;
	}
	public EgocentricDirection getLastGoalDir() {
		return myLastGoalDir;
	}
	public EgocentricDirection getLastGoalDeltaDir() {
		return myLastGoalDeltaDir;
	}
	protected Double getSlackDegrees(GazeDimension d) {
		if (myGazeStrategy == null) {
			return null;
		}
		if (d == GazeDimension.HORIZONTAL) {
			return myGazeStrategy.getSlackHorizDeg();
		} else if (d == GazeDimension.VERTICAL) {
			return myGazeStrategy.getSlackVertDeg();
		}
		return null;
	}	
}
