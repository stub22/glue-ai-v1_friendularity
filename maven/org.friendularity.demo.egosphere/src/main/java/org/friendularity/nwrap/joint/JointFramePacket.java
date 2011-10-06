/*
 *  Copyright 2008 Hanson Robotics Inc.
 *  All Rights Reserved.
 */

package org.friendularity.nwrap.joint;

import org.friendularity.nwrap.packet.StructPacket;

/**
 *
 * @author Stu Baurmann
 */
public class JointFramePacket extends StructPacket {
	public			JointFrameStruct		frame = inner(new JointFrameStruct());
	
	protected void initAfterMapped() {
		System.out.println("JFP initAfterMapped()");
		super.initAfterMapped();
		frame.initAfterMapped();
	}
}
