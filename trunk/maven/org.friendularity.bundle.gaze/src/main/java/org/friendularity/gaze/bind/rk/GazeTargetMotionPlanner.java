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
package org.friendularity.gaze.bind.rk;


import org.cogchar.api.animoid.gaze.IGazeTarget;
import org.mechio.api.motion.Robot.RobotPositionMap;

/**
 * This class is a stub showing how a Java service approach to gaze planning is constructed.
 */
public class GazeTargetMotionPlanner implements 
        GazePlanner<IGazeTarget, RobotPositionMap> {

    @Override
    public RobotPositionMap getMovements(long time, long interval, 
    IGazeTarget tracker, RobotPositionMap currentPos) {
        throw new UnsupportedOperationException("Not supported yet.");
    }
    
}
