/*
 *  Copyright 2008 Hanson Robotics Inc.
 *  All Rights Reserved.
 */

package org.friendularity.nwrap.joint;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author Stu Baurmann
 */
public enum JointCommand implements Serializable {
	EMPTY,
	MOVE_ABSOLUTE_IMMEDIATE,
	MOVE_ABSOLUTE_LINEAR_RAMP,
	MOVE_RELATIVE_IMMEDIATE,
	MOVE_RELATIVE_LINEAR_RAMP,
	NOTIFY_CURRENT_POSITION;

	private	static	List<JointCommand>		theJointCommandList;	

	public static	List<JointCommand>		getJointCommandList() {
		if (theJointCommandList == null) {
			theJointCommandList = new ArrayList<JointCommand>();
			// Order here must be the same as order in the Enum decl, and 
			// as the order of integer constants on the C++ side.
			theJointCommandList.add(JointCommand.EMPTY);
			theJointCommandList.add(JointCommand.MOVE_ABSOLUTE_IMMEDIATE);	
			theJointCommandList.add(JointCommand.MOVE_ABSOLUTE_LINEAR_RAMP);			
			theJointCommandList.add(JointCommand.MOVE_RELATIVE_IMMEDIATE);	
			theJointCommandList.add(JointCommand.MOVE_RELATIVE_LINEAR_RAMP);
			theJointCommandList.add(JointCommand.NOTIFY_CURRENT_POSITION);
		}
		return theJointCommandList;
	}	
}
