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

import java.util.ArrayList;
import java.util.List;
import java.util.Set;



import org.cogchar.api.animoid.protocol.Frame;

import org.cogchar.api.animoid.protocol.JointPosition;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * @author Stu B. <www.texpedient.com>
 */
public class GazeAnimationBuilder {
	private static Logger	theLogger = LoggerFactory.getLogger(GazeAnimationBuilder.class.getName());


	public static Frame makeGazeCenteringFrame(List<GazeJoint> gazeJoints) {
		Frame frame = new Frame();
		for (GazeJoint gj: gazeJoints) {
			JointPosition jp = gj.getJoint().getCenterPosition();
			frame.addPosition(jp);
		}
		/*
		frame.addPosition(makeCenteringJointPosition(r, MuscleJoint.BothEyes_Up.name()));
		frame.addPosition(makeCenteringJointPosition(r, MuscleJoint.BothEyes_TurnRight.name()));
		frame.addPosition(makeCenteringJointPosition(r, MuscleJoint.Head_TurnRight.name()));
		frame.addPosition(makeCenteringJointPosition(r, MuscleJoint.UpperNod_Forward.name()));
		*/
		return frame;
	}

}
