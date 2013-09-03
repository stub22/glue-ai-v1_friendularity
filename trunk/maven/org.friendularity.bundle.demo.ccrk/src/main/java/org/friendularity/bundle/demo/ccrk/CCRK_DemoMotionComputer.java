/*
 *  Copyright 2013 by The Friendularity Project (www.friendularity.org).
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

package org.friendularity.bundle.demo.ccrk;

import org.cogchar.bind.rk.robot.motion.CogcharMotionComputer;
import org.cogchar.bind.rk.robot.motion.CogcharMotionSource;
import org.friendularity.api.west.WorldEstimate;
import org.robokind.api.common.position.NormalizedDouble;
import org.robokind.api.motion.Joint;
import org.robokind.api.motion.Robot;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Stu B. <www.texpedient.com>
 */

public class CCRK_DemoMotionComputer extends CogcharMotionComputer {
	long myCycleCount = 0;
	static Logger theLogger = LoggerFactory.getLogger(CogcharMotionSource.class);
	private WorldEstimate myWorldEstimate;
	public void setWorldEstimate(WorldEstimate we) {
		myWorldEstimate = we;
	}
	int sinbadJointNums[] = {
			42, 100,
			200, 201, 202, 
			300, 301, 
			310, 311, 312, 
			320, 321, 322,
			400, 401, 410, 411, 420,
			500, 501, 510, 511, 520,
			600, 601, 602, 610, 620, 621,
			700, 701, 702, 710, 720, 721
	};

	@Override public void notifySourceComputingCycle(CogcharMotionSource source, long currentTimeUTC, long moveLengthMilliSec) {
		myCycleCount++;
		Robot srcBot = source.getRobot();
		Robot.Id srcBotID = srcBot.getRobotId();
		String robotIdString = srcBotID.getRobtIdString();
		Robot.RobotPositionMap rpm = srcBot.getCurrentPositions();
		/*
		if ((myCycleCount % 100) == 1) {
			theLogger.info("notify[cycle=" + myCycleCount + ", currentTime" + currentTimeUTC + ", moveLen=" 
					+ moveLengthMilliSec + ", src=" + source + ", botID=" + srcBotID + ", curPosMap=" + rpm + "]");
		}
		*/
	
		if (robotIdString.equals("Sinbad")) {
			int jointCount = sinbadJointNums.length;
			// int spineJointNum = 42;
			

			double zeroToTwoCycle = (myCycleCount % 250) / 125.0;
			long loopCount = myCycleCount / 250;
			int jointIndex = (int) (loopCount / 3) % jointCount;
			int someJointNum = sinbadJointNums[jointIndex];
			Joint.Id someJointID = new Joint.Id(someJointNum);
			Robot.JointId someRJID = new Robot.JointId(srcBotID, someJointID);
			NormalizedDouble oldJPos = rpm.get(someRJID);
			
			NormalizedDouble nextJPos = new NormalizedDouble (0.5 + 0.5 * Math.sin(Math.PI * zeroToTwoCycle));
			Robot.RobotPositionHashMap goalPosMap = new Robot.RobotPositionHashMap();
			goalPosMap.put(someRJID, nextJPos);
			source.move(goalPosMap, moveLengthMilliSec);
			if ((myCycleCount % 200) == 1) {
				theLogger.info("notify[cycle=" + myCycleCount + ", currentTime" + currentTimeUTC + ", moveLen="
					+ moveLengthMilliSec + ", src=" + source + ", botID=" + srcBotID + ", joinID" + someRJID 
					+ ", oldJPos=" + oldJPos + ", nextJPos=" + nextJPos + ", curPosMap=" + rpm + "]");
			}
		}		
	}
	public void sinJoints() { 
				/*
				 * 
HominoidBodySchema
* 
42	Root_Spin
100	Waist_Yaw
200	Neck_Yaw
201	Neck_Roll
202	Neck_Pitch
300	Brows_Up
301	Blink_Open
310	Eyes_Vert
	
311	LtEye_Yaw
312	RtEye_Yaw
322	Jaw
320	LtSmile
321	RtSmile
400	LtShoulder_Pitch
401	LtShoulder_Roll
410	LtElbow_Yaw
411	LtElbow_Pitch
420	LtWrist_Yaw
500	RtShoulder_Pitch
501	RtShoulder_Roll
510	RtElbow_Yaw
511	RtElbow_Pitch
520	RtWrist_Yaw
600	LtHip_Roll
601	LtHip_Yaw
602	LtHip_Pitch
610	LtKnee_Pitch
620	LtAnkle_Pitch
621	LtAnkle_Roll
700	RtHip_Roll
701	RtHip_Yaw
702	RtHip_Pitch
710	RtKnee_Pitch
720	RtAnkle_Pitch
721	RtAnkle_Roll
				 */
	}

}
