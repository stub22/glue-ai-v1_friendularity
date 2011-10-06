/*
 *  Copyright 2008 Hanson Robotics Inc.
 *  All Rights Reserved.
 */

package org.friendularity.app.animation;

import org.friendularity.nwrap.blend.InternalBlendingFacade;
import org.friendularity.nwrap.joint.ITransformJointFrame;
import org.friendularity.nwrap.joint.JointBoundary;
import org.friendularity.nwrap.joint.JointCommand;
import org.friendularity.nwrap.joint.JointFramePacket;
import org.friendularity.nwrap.joint.JointFrameStruct;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;
import org.cogchar.animoid.protocol.JointPositionSnapshot;
import org.cogchar.animoid.protocol.JointStateCoordinateType;
import org.cogchar.animoid.protocol.Robot;

/**
 *
 * @author Stu Baurmann
 */
public class JointFrameTransformer implements ITransformJointFrame {
	private static Logger	theLogger = Logger.getLogger(JointFrameTransformer.class.getName());
	private JointFrameStruct			myInputJFS, myOutputJFS;
	// Probably going to insert a more abstract type for the delegate.
	private BlendingAnimator			myDelegate;
	

	

	public JointFrameTransformer(JointFrameStruct inJFS, JointFrameStruct outJFS) {
		myInputJFS = inJFS;
		myOutputJFS = outJFS;
	}
	public synchronized void setDelegate(BlendingAnimator ba) {
		myDelegate = ba;
	}

	public static JointFrameTransformer setup(InternalBlendingFacade ibf) throws Throwable {
		JointFramePacket inFramePacket = new JointFramePacket();
		inFramePacket.mapAndAllocate();
		JointFrameStruct inFrame = inFramePacket.frame;

		JointFramePacket outFramePacket = new JointFramePacket();
		outFramePacket.mapAndAllocate();		
		JointFrameStruct outFrame = outFramePacket.frame;
		
		JointFrameTransformer transformer = new JointFrameTransformer(inFrame, outFrame);
		JointBoundary.registerJointFrameTransformer("ROLY-POLY",   transformer, 
				inFramePacket, outFramePacket, ibf);
		return transformer;
	}
	public JointFrameStruct getInputJFS() {
		return  myInputJFS;
	}
	public JointFrameStruct getOutputJFS() {
		return  myOutputJFS;
	}
	public synchronized JointPositionSnapshot makeLopsidedFrameSnapshotFromJFS(JointFrameStruct jfs) {
		// Translate native-compatible JointFrameStruct into an animoid-protocol
		// Frame, but retain native coordinates.
		Map<Integer, Double>	positionValuesByJointID = new HashMap<Integer,Double> ();
		jfs.snapshotMatchingValuesIntoMap(JointCommand.NOTIFY_CURRENT_POSITION,
					positionValuesByJointID);
		Robot robot = myDelegate.getAnimoidFacade().getMainRobot();

		return new JointPositionSnapshot(robot,
					JointStateCoordinateType.FLOAT_ABS_LOPSIDED_PIECEWISE_LINEAR,
					positionValuesByJointID);

	}


	public void transform() {
		// theLogger.finer("TestJointFrameTransformer.transform()");
		myDelegate.doAnimationTransform(myInputJFS, myOutputJFS);
	}
}
