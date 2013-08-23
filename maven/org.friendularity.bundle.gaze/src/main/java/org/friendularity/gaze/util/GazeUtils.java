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

package org.friendularity.gaze.util;

import org.friendularity.sight.track.IGazeTarget;
import org.cogchar.api.animoid.protocol.EgocentricDirection;
import org.cogchar.api.animoid.protocol.SmallAngle;

/**
 * @author Matt Stevenson
 */
public class GazeUtils {
	public static SmallAngle computeGazeDistanceAngle(IGazeTarget a, IGazeTarget b){
		if(a == null || b == null){
			return null;
		}
		EgocentricDirection edA = a.getEstimatedDirection();
		EgocentricDirection edB = b.getEstimatedDirection();
		if(edA == null || edB == null){
			return null;
		}
		return edA.computeDistanceAngle(edB);
	}
	public static Double computeGazeDistanceDeg(IGazeTarget a, IGazeTarget b){
		SmallAngle dist = computeGazeDistanceAngle(a, b);
		if(dist == null){
			return null;
		}
		return dist.getDegrees();
	}
}
