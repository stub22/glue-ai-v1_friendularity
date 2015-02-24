/*
 *  Copyright 2008 Hanson Robotics Inc.
 *  All Rights Reserved.
 */

package org.friendularity.nwrap.joint;

/**
 * @author Stu Baurmann
 */
public interface ITransformJointFrame {
	// Transforms from pre-arranged input frame buf into a pre-arranged output frame buf
	public void transform();
}
