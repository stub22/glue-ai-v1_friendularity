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
import java.util.Set;

import org.friendularity.gaze.estimate.GazeJointStateSnap;
import org.cogchar.api.animoid.config.bonus.AnimoidConfig;

import org.cogchar.animoid.job.MotionJob;
import org.cogchar.api.animoid.protocol.Frame;
import org.cogchar.api.animoid.protocol.JVFrame;
import org.cogchar.api.animoid.protocol.Joint;
import org.cogchar.api.animoid.protocol.JointVelocityAROMPS;
import org.friendularity.gaze.api.GazeJointStrategy;
import org.friendularity.gaze.util.GazeStrategyCue;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


/**
 * @author Stu B. <www.texpedient.com>
 * 
 * Typically a RestoringForceJob will also attempt to achieve/keep eyes centered in head.
 */
public class RestoringForceJob extends MotionJob {
	private static Logger	theLogger = LoggerFactory.getLogger(RestoringForceJob.class.getName());
	
	public	JVFrame							myLastVelFrame;
	private	GazeStrategyCue					myHoldAndRecenterStrategy;

	public RestoringForceJob(AnimoidConfig aconf) { 
		super(aconf);
	}
	public void setHoldAndRecenterStrategy(GazeStrategyCue harStrat) {
		theLogger.info("Setting holdAndRecenterStrategy to: " + harStrat);
		myHoldAndRecenterStrategy = harStrat;
	}
	@Override public JVFrame contributeVelFrame(
			Frame prevPosAbsRomFrame,
			JVFrame prevVelRomFrame,
			Set<Joint> cautionJoints) {

		JVFrame velFrame = new JVFrame();

        if(this.getStatus() == Status.RUNNING) {
			if (myHoldAndRecenterStrategy != null) {
				List<GazeJointStrategy> gjsList = myHoldAndRecenterStrategy.getJointLinks();
				for (GazeJointStrategy gjs : gjsList) {
					GazeJointStateSnap stat = new GazeJointStateSnap(gjs, prevPosAbsRomFrame, prevVelRomFrame);
					double worldPosDeg = stat.getWorldPosDeg();
					double worldPosMagDeg = Math.abs(worldPosDeg);
					Double recenterSlackDeg = gjs.recenterSlackDeg;
					if (recenterSlackDeg != null) {
						if (worldPosMagDeg < recenterSlackDeg) {
							// theLogger.info("Recentering joint " + stat.getShortDescriptiveName()
							//			+ " worldPosDeg=" + worldPosDeg + " is within slack: "
							//			+ recenterSlackDeg);
							continue;
						}
					}
					Double recenterMaxVelDPS = gjs.recenterMaxVelDPS;
					double secondsPerFrame = getTimeKeeper().getNominalSecPerFrame();
					double oneFrameJumpVelDPS = -1.0 * worldPosDeg  / secondsPerFrame;
					double oneFrameJVMagDPS = Math.abs(oneFrameJumpVelDPS);
					double velDPS = oneFrameJumpVelDPS;
					if ((recenterMaxVelDPS != null) && (oneFrameJVMagDPS > recenterMaxVelDPS)) {
						velDPS = Math.signum(oneFrameJumpVelDPS) * recenterMaxVelDPS;
					}
					double velROM = velDPS / stat.getTotalRomDegrees();
					JointVelocityAROMPS jvel = new JointVelocityAROMPS(stat.getJoint(), velROM);
					velFrame.addPosition(jvel);
				}
            }
        }
		myLastVelFrame = velFrame;
		return velFrame;
	}

    public void setRunning(){
        setStatus(Status.RUNNING);
    }
    public void setPaused(){
        setStatus(Status.PAUSED);
    }
    public void setCompleted(){
        setStatus(Status.COMPLETED);
    }
	@Override public String getContentSummaryString() {
		return "lastVelFrame=" + myLastVelFrame;
	}
}
