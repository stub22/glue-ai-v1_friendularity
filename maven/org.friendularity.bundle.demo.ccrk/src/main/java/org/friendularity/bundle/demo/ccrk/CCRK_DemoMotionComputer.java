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
import org.robokind.api.motion.Robot;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Stu B. <www.texpedient.com>
 */

public class CCRK_DemoMotionComputer extends CogcharMotionComputer {
	long myCycleCount = 0;
	static Logger theLogger = LoggerFactory.getLogger(CogcharMotionSource.class);
	@Override public void notifySourceComputingCycle(CogcharMotionSource source, long currentTimeUTC, long moveLengthMilliSec) {
		myCycleCount++;
		Robot srcBot = source.getRobot();
		Robot.Id srcBotID = srcBot.getRobotId();
		String robotIdString = srcBotID.getRobtIdString();
		Robot.RobotPositionMap rpm = srcBot.getCurrentPositions();
		if ((myCycleCount % 100) == 1) {
			theLogger.info("notify[cycle=" + myCycleCount + ", currentTime" + currentTimeUTC + ", moveLen=" 
					+ moveLengthMilliSec + ", src=" + source + ", botID=" + srcBotID + ", curPosMap=" + rpm + "]");
		}

		
	}

}
