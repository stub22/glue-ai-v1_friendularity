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

import org.freckler.sight.track.IGazeTarget;
import org.cogchar.api.animoid.protocol.EgocentricDirection;
import org.cogchar.api.animoid.protocol.Frame;
import org.cogchar.api.animoid.protocol.SmallAngle;

/**
 *
 * @author Matt Stevenson
 */
public class SimpleGazeTarget implements IGazeTarget {

	private EgocentricDirection myTargetDirection;

	public Flavor getCurrentFlavor() {
		return Flavor.EGOCENTRIC_DIRECTION;
	}
	
	public SimpleGazeTarget(double azimuth, double elavation){
		SmallAngle az = SmallAngle.makeFromDeg(azimuth);
		SmallAngle ev = SmallAngle.makeFromDeg(elavation);
		myTargetDirection = new EgocentricDirection(az, ev);
	}
	public SimpleGazeTarget(EgocentricDirection targetDirection){
		myTargetDirection = targetDirection;
	}
	public EgocentricDirection getEstimatedDirection() {
		return myTargetDirection;
	}
	public void setTargetDirection(EgocentricDirection targetDirection) {
		myTargetDirection = targetDirection;
	}

	public Frame getEstimatedServoSnapshot() {
		throw new UnsupportedOperationException("SimpleGazeTarget does not support ServoSnapshots");
	}
	@Override public Double getVergenceAngle(Double defaultWidth, Double slope) {
        return 0.0;
	}
	public void notifyAttentionStarted() { }
	public void notifyAttentionConfirmed() { }
	public void notifyAttentionStopped() { }
}
