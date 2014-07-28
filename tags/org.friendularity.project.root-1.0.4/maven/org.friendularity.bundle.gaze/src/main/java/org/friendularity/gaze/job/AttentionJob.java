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


import org.friendularity.gaze.job.RestoringForceJob;
import org.cogchar.api.animoid.config.bonus.AnimoidConfig;
// import org.cogchar.animoid.gaze.GazeStrategyCue;
// import org.cogchar.animoid.gaze.IGazeTarget;
import org.cogchar.animoid.job.AnimoidJob;
import org.cogchar.animoid.job.BlenderJob;
import org.cogchar.api.animoid.protocol.EgocentricDirection;
import org.cogchar.api.animoid.protocol.Frame;
import org.cogchar.zzz.platform.stub.JobSpaceStub;
import org.friendularity.gaze.api.AnimoidGazeConfig;
import org.friendularity.gaze.estimate.GazeDirectionComputer;
import org.friendularity.gaze.util.GazeStrategyCue;
import org.freckler.sight.impl.hypo.SightModel;
import org.cogchar.api.animoid.gaze.IGazeTarget;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


/**
 *
 * @author Stu B. <www.texpedient.com>
 */
public class AttentionJob extends AnimoidJob {
	private static Logger	theLogger = LoggerFactory.getLogger(AttentionJob.class.getName());

	private		GotoGazeJob				myGotoGazeJob;
    private     RestoringForceJob		myRestoringJob;
	private		BlenderJob				myParentJob;
	private		SightModel				mySightModelIsUnused;

	private		GazeStrategyCue			mySavedGotoGazeStrategy, myHoldAndRecenterStrategy;
	private		boolean					myHoldingFlag = false;

	public AttentionJob(BlenderJob bj, SightModel fm, AnimoidGazeConfig aconf, JobSpaceStub jobSpace) {
		super(aconf);
		myParentJob = bj;
		mySightModelIsUnused = fm;
		myGotoGazeJob = new GotoGazeJob(aconf);
		
		myGotoGazeJob.setGazeDirectionComputer((GazeDirectionComputer) fm.getGazeDirectionComputer());
		myRestoringJob = new RestoringForceJob(aconf);
		GazeStrategyCue initialStrategy = aconf.getDefaultGazeStrategy();
		mySavedGotoGazeStrategy = initialStrategy;
		myGotoGazeJob.setGazeStrategy(initialStrategy);
		myHoldingFlag = false;
		myRestoringJob.setPaused();
		myParentJob.registerMotionJob(myGotoGazeJob);
		jobSpace.postManualJob(myGotoGazeJob);
		myParentJob.registerMotionJob(myRestoringJob);
		jobSpace.postManualJob(myRestoringJob);
	}
	public void suggestGazeStrategy(GazeStrategyCue gsc) {
		mySavedGotoGazeStrategy = gsc;
		stopHolding();
	}
	public void suggestHoldAndRecenterStrategy(GazeStrategyCue harStrat) {
		myHoldAndRecenterStrategy = harStrat;
		myRestoringJob.setHoldAndRecenterStrategy(harStrat);
	}
	public void suggestAttentionTarget(IGazeTarget target) {
		IGazeTarget oldTarget = myGotoGazeJob.getGazeTarget();
		if (oldTarget != null) {
			oldTarget.notifyAttentionStopped();
		}
		myGotoGazeJob.setGazeTarget(target);
		if (target  != null) {
			target.notifyAttentionStarted();
		}
		stopHolding();
	}
	public boolean getHoldingStatusFlag() {
		return myHoldingFlag;
	}
	public IGazeTarget getAttentionTarget() {
		return myGotoGazeJob.getGazeTarget();
	}
	public GazeStrategyCue getGazeStrategy() {
		return mySavedGotoGazeStrategy;
	}

	public AnimoidGazeConfig getAnimoidGazeConfig() {
		return (AnimoidGazeConfig) getAnimoidConfig();
	}
    public synchronized void rebalanceGazeJobs(){
		// Note - any change in center-helper Strategy takes place on the next startHold().
		// If we are already holding, that strategy remains in place.
		// This can be changed if it causes problems.
		if(this.getStatus() != Status.RUNNING) {
			theLogger.trace("Gaze attention is disabled, so nothing to do");
			return;
		}
		EgocentricDirection lastGotoDeltaDir = myGotoGazeJob.getLastGoalDeltaDir();
		GazeStrategyCue currStrategy = myGotoGazeJob.getGazeStrategy();
		IGazeTarget currTarget = myGotoGazeJob.getGazeTarget();
		boolean currentGazeIsOn = GazeStrategyCue.isActiveStrategy(currStrategy);
		boolean savedGazeIsOn =  GazeStrategyCue.isActiveStrategy(mySavedGotoGazeStrategy);
		boolean gazeIsFullyOn = currentGazeIsOn && (currTarget != null);
		if (lastGotoDeltaDir != null) {
			double lgdAzDeg = lastGotoDeltaDir.getAzimuth().getDegrees();
			double lgdElDeg = lastGotoDeltaDir.getElevation().getDegrees();
			double lgdNorm = Math.sqrt(lgdAzDeg * lgdAzDeg + lgdElDeg * lgdElDeg);
			// Switch on holding when we get close enough
			Double holdEntryThresh = getAnimoidGazeConfig().holdEntryNormDeg;
			if ((!myHoldingFlag) && gazeIsFullyOn 
					&& (holdEntryThresh != null) && (lgdNorm <= holdEntryThresh)) {
				startHolding();
			}
			// Switch off holding when we get far enough
			Double holdExitThresh = getAnimoidGazeConfig().holdExitNormDeg;
			if (myHoldingFlag) {
				if ((holdExitThresh != null) && (lgdNorm >= holdExitThresh) || !savedGazeIsOn) {
					stopHolding();
				} 
			}
		} else {
			theLogger.trace("Attention rebalance got null last-goal-delta, doing nothing");
		}
	}
	private synchronized void startHolding() {
		// What about when current gotoStrategy is null or something like NO_GAZE?
		theLogger.debug("******** Gaze Hold START ******");
		if (myHoldAndRecenterStrategy != null) {
			GazeStrategyCue helperStrategy = myHoldAndRecenterStrategy.getHelperStrategy();
			myGotoGazeJob.setGazeStrategy(helperStrategy);
			myRestoringJob.setRunning();
			myHoldingFlag = true;
		} else {
			theLogger.warn("Can't start holding because myHoldAndRecenterStrategy=null");
		}

	}
	private synchronized void stopHolding() {
		// Has the important side effect of restoring/setting gotoGazeStrategy.
		theLogger.debug("******** Gaze Hold END ******");
		myHoldingFlag = false;
		myRestoringJob.setPaused();
		if (mySavedGotoGazeStrategy != null) {
			myGotoGazeJob.setGazeStrategy(mySavedGotoGazeStrategy);
		} else {
			theLogger.warn("Can't fully stop holding because mySavedGotoGazeStrategy=null");
		}
	}
	public String getContentSummaryString() {
		return "holdStatus=" + myHoldingFlag
			+ ", regularStrategy=" + mySavedGotoGazeStrategy
			+ ", hold/recentering strategy=" + myHoldAndRecenterStrategy;
	}	
	public String  getTypeString() {
		return "AttentionJob";
	}
	public String getAttentionDebugText() {
		GazeJob activeGazeJob = myGotoGazeJob;
		EgocentricDirection goalDir = activeGazeJob.getLastGoalDir();
		EgocentricDirection goalDeltaDir = activeGazeJob.getLastGoalDeltaDir();
		Frame lastRestoringFrame = myRestoringJob.myLastVelFrame;
		String debugText =
			"holdStatus=" + getHoldingStatusFlag()
			+ "\n\nrestoringFrame=" + lastRestoringFrame
			+ "\n\ngoalDir=" + goalDir
			+ "\n\ngoalDeltaDir=" + goalDeltaDir
			+ "\n\nlastNominalYieldDPS(az,el)=" + activeGazeJob.myLastHorizYieldDPS + ", " + activeGazeJob.myLastVertYieldDPS
			+ "\nlastNormRDCF=" + activeGazeJob.myLastNormRDCF
			+ "\n\ngazeTarget=" + activeGazeJob.getGazeTarget()
			+ "\n\nactiveGazeStrategy=" + activeGazeJob.getGazeStrategy();
		return debugText;
	}

	@Override public void enableMotion() {
		myGotoGazeJob.enableMotion();
		super.enableMotion();
	}
	@Override public void disableMotion() {
		stopHolding();
		myGotoGazeJob.disableMotion();
		super.disableMotion();
	}

}
