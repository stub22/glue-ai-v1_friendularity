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
package org.friendularity.gaze.bind.mio;

import org.mechio.api.motion.protocol.JointPositionMap;

/**
 * A GazePlanner plans the movements towards a Target
 * @param <Target> Type of object the GazePlanner can move towards
 * @param <PosMap> Type of JointPositionMap for current positions and returned
 * movements.  (usually a RobotPositionMap)
 * 
 * @author Matthew Stevenson
 */
public interface GazePlanner<Target, PosMap extends JointPositionMap> {
    /**
     * Returns a set of goal positions to move towards over the given interval.
     * @param curTimeUTC the current time
     * @param intervalMilliSec the expected length of the movement to plan
     * @param target the target to move towards
     * @param currentPos the Joint positions at the current time
     * @return set of goal positions to move towards over the given interval
     */
    public PosMap getMovements(long curTimeUTC, long intervalMilliSec, 
            Target target, PosMap currentPos);    
}
