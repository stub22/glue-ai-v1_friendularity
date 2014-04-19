/*
 *  Copyright 2008 Hanson Robotics Inc.
 *  All Rights Reserved.
 */

package org.friendularity.nwrap.joint;

import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;
import javolution.io.Struct;
import javolution.io.Struct.Enum32;
import javolution.io.Struct.Float64;
import javolution.io.Struct.Unsigned32;

/**
 * @author Stu Baurmann
 */
public class JointInstructionStruct extends Struct { 
	private static Logger	theLogger = Logger.getLogger(JointInstructionStruct.class.getName());	
		
	public final		Enum32				jointCommand		= null; // new Enum32(JointCommand.getJointCommandList());
	public final		Unsigned32			logicalJointID		= new Unsigned32();	
	public final		Float64				targetJointValue	= new Float64();
	public final		Float64				moveTimeSeconds		= new Float64();
	// Pad structure out to 32 bytes
	public final		Unsigned32			dummy01				= new Unsigned32();			
	public final		Unsigned32			dummy02				= new Unsigned32();			

	public void initAfterMapped() {
		theLogger.finest("JointInstruction.initAfterMapped()--My byte order is: " + this.byteOrder());
		clear();
	}
	public void setLogicalJointID(int ljid) {
		logicalJointID.set(ljid);
	}
	public void setJointCommand(JointCommand jc) {
		jointCommand.set(jc);		
	}
	public void setTargetJointValue(double tjv) {
		targetJointValue.set(tjv);
	}
	public void clear() {
		jointCommand.set(JointCommand.EMPTY);
		logicalJointID.set(-9999);
		targetJointValue.set(-1000.0f);
		moveTimeSeconds.set(-1000.0f);		
	}
	
}
