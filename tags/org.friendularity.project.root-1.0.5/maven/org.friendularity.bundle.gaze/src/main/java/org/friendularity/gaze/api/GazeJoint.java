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

package org.friendularity.gaze.api;

import org.cogchar.api.animoid.world.WorldJoint;


/**
 * @author Stu B. <www.texpedient.com>
 */

public class GazeJoint extends WorldJoint {

	public enum Direction {
		LEFT,
		RIGHT,
		UP,
		DOWN
		// Other directions could be added here, e.g. to represent head-tilt, without affecting the rest of the class.
		// (But GazePlan would need some work to restore consistency, since it assumes all links are either horiz or vert).
	}
	private		Direction	positiveDirection;

	public Direction getPositiveDirection() {
		return positiveDirection;
	}

	public boolean isPixelNumberSensePositive() {
		return ((positiveDirection == Direction.RIGHT) || (positiveDirection == Direction.DOWN));
	}
	public boolean isHorizontal() {
		return ((positiveDirection == Direction.LEFT) || (positiveDirection == Direction.RIGHT));
	}
	public boolean isEgocentricDirectionSensePositive() {
		return ((positiveDirection == Direction.RIGHT) || (positiveDirection == Direction.UP));
	}

	@Override public boolean isWorldSenseInverted() {
		return !isEgocentricDirectionSensePositive();
	}


	public String toString() {
		return "\nGazeJoint["
				+ "\npositiveDirection=" + getPositiveDirection()
				+ "\nsuper=" + super.toString()
				+ "]";
	}
}
