/*
 *  Copyright 2008 Hanson Robotics Inc.
 *  All Rights Reserved.
 */

package org.friendularity.model.joint;

/**
 * @author Stu Baurmann
 */
public class OldJointModel {
	// Default=Center angle is defined as 0 radians
	public		double		minAngleRad;	// corresponds to -100% = normal 0.0
	public		double		maxAngleRad;	// corresponds to +100% = normal 1.0
	public		String		jointName;		// 
	public		int			jointID;
}
