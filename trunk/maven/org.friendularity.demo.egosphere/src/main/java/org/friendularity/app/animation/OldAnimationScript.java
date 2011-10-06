/*
 *  Copyright 2008 Hanson Robotics Inc.
 *  All Rights Reserved.
 */

package org.friendularity.app.animation;

import org.friendularity.nwrap.joint.JointAnimationPacket;
import org.friendularity.nwrap.joint.JointBoundary;
import org.friendularity.nwrap.joint.JointFrameStruct;

import org.friendularity.nwrap.joint.JointInstructionStruct;
import java.io.Serializable;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.cogchar.animoid.config.ServoChannelConfig;

/**
 * @author Stu Baurmann
 */
public class OldAnimationScript implements Serializable {
	private static Logger	theLogger = Logger.getLogger(OldAnimationScript.class.getName());
	
	private	transient JointAnimationPacket							myJAPacket;
	
	private Map<ServoChannelConfig, OldServoChannelScript>		myChannelScriptsByConfig;
	
	public OldAnimationScript(JointAnimationPacket japacket) {
		myJAPacket = japacket; 
		myChannelScriptsByConfig = new HashMap<ServoChannelConfig, OldServoChannelScript>();		
	}
	public void buildChannelScripts(ServoChannelConfig[] configArray) {
		int frameCount = (int) myJAPacket.frameCount.get();
		List<Map<Integer,JointInstructionStruct>> logicalFrameMapList = myJAPacket.getLogicalFrameMaps();
		HashSet<Integer> usedLogicalChannels = new HashSet<Integer>();
		for (Map<Integer,JointInstructionStruct> m : logicalFrameMapList) {
			usedLogicalChannels.addAll(m.keySet());
		}
		for (int logJointID : usedLogicalChannels) {
			ServoChannelConfig scc = ServoChannelConfig.findConfigForLogicalChannel(configArray, logJointID);
			OldServoChannelScript scs = new OldServoChannelScript(scc, frameCount, getName());
			for (int i=0; i < frameCount; i++) {
				Map<Integer,JointInstructionStruct> frameMap = logicalFrameMapList.get(i);
				JointInstructionStruct instruction = frameMap.get(logJointID);
				if (instruction != null) {
					scs.absorbInstruction(i, instruction);
				}
			}
			myChannelScriptsByConfig.put(scc, scs);
		}
	}
	public Collection<OldServoChannelScript> getChannelScripts() {
		return myChannelScriptsByConfig.values();
	}
	public String getName() {
		return myJAPacket.animationName.get();
	}
	public int getFrameCount() {
		return (int) myJAPacket.frameCount.get();
	}
	public int getChannelCount() { 
		return myChannelScriptsByConfig.size();
	}
	public  JointFrameStruct getRawFrameAtIndex(int frameIndex) {
		return myJAPacket.frames[frameIndex];
	}
	public void sendToNativeBlendRule() {
		theLogger.log(Level.WARNING, "AnimationScript ignoring request to send data to native blend rule");
		// No longer used!
		// JointBoundary.sendJointAnimationPacket(myJAPacket);
	}
	public void destroy() {
		if (myJAPacket != null) {
			try {
				myJAPacket.unmapAndDestroy();
			} catch (Throwable t) {
				theLogger.log(Level.SEVERE, "AnimationScript.destroy() caught exception while destroying packet", t);
			}
			myJAPacket = null;
		}
	}
	public static void testJointAnimationPacketStuff() {
		JointAnimationPacket jap = new JointAnimationPacket(2);
		try {
			jap.mapAndAllocate();
			jap.animationCommand.set(JointAnimationPacket.JointAnimationCommand.ADD_NAMED_TO_LIBRARY);
			jap.animationName.set("theMacarena");
			jap.frames[0].addAbsoluteImmediateMove(14, 77.0);
			jap.frames[0].addAbsoluteImmediateMove(3, -19.2);
			String hexStringBefore = jap.toString();
			theLogger.fine("jap-hex before :\n" + hexStringBefore);
			JointBoundary.sendJointAnimationPacket(jap);
			String hexStringAfter = jap.toString();
			theLogger.fine("jap-hex after :\n" + hexStringAfter);
			jap.unmapAndDestroy();
		} catch (Throwable t) {
			theLogger.log(Level.SEVERE, "Error in testJointAnimationPacketStuff", t);
		}
	}		
}
