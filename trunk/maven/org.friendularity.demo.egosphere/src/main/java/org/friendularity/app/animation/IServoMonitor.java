/*
 *  Copyright 2008 Hanson Robotics Inc.
 *  All Rights Reserved.
 */

package org.friendularity.app.animation;

import org.cogchar.animoid.protocol.JointPositionSnapshot;

/**
 * @author Stu Baurmann
 */
public interface IServoMonitor {
	public void servoSnapshotUpdate(JointPositionSnapshot lopsidedSnapshot);
}
