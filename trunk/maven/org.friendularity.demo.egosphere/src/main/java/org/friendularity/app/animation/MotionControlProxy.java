/*
 *  Copyright 2008 Hanson Robotics Inc.
 *  All Rights Reserved.
 */

package org.friendularity.app.animation;

import org.friendularity.gui.blending.BlendingMonitorBean;
import java.util.logging.Logger;
import org.cogchar.animoid.broker.MotionController;
import org.cogchar.animoid.broker.MotionController.Component;


/**
 * @author Stu Baurmann
 * 
 * 
 */
public class MotionControlProxy implements MotionController {
	private static Logger	theLogger = Logger.getLogger(MotionControlProxy.class.getName());
	BlendingMonitorBean		myBMB;
	public MotionControlProxy(BlendingMonitorBean bmb) {
		myBMB = bmb;
	}
	@Override public void setComponentState(Component c, boolean activationState) {
		if (c == MotionController.Component.FACE_ATTENTION) {
			setGazeState(activationState);
		}
	}
	@Override public void setComponentValue(Component c, Integer value) {
		if (c == MotionController.Component.FACE_ATTENTION) {
			setGazeSightNumber(value);
		}
	}	
	@Override public void setComponentValue(Component c, String value) {
		if (c == MotionController.Component.GAZE_PLAN) {
			setGazePlanName(value);
		}
	}

	@Override public void setGazeState(boolean enabled) {
		myBMB.setUsingAnyFaceAttentionRule(enabled);
	}

	@Override public void setGazePlanName(String planName) {
		myBMB.setGazeStrategyName(planName);
	}

	@Override public void setGazeSightNumber(Integer sightNumber) {
		myBMB.setGazeFaceNumber(sightNumber);		
	}



}
