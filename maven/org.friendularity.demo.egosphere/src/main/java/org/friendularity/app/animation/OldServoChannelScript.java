/*
 *  Copyright 2008 Hanson Robotics Inc.
 *  All Rights Reserved.
 */

package org.friendularity.app.animation;

import org.friendularity.nwrap.joint.JointCommand;
import org.friendularity.nwrap.joint.JointFrameStruct;
import org.friendularity.nwrap.joint.JointInstructionStruct;
import java.io.Serializable;
import java.util.logging.Logger;
import org.cogchar.animoid.config.ServoChannelConfig;

/**
 *
 * @author Stu Baurmann
 */
public class OldServoChannelScript implements Serializable {
	private static Logger	theLogger = Logger.getLogger("com.hansonrobotics.app.animation.ServoChannelScript");
	
	private	ServoChannelConfig		myChannelConfig;
	
	public static class Command implements Serializable {
		public JointCommand			jointCommand;
		// Values in the range [-1.0 to +1.0] - but soon to be [0.0 to 1.0]
		public double				positionValue;
	}					
	
	// private static double LOGICAL_DEFAULT_POSITION = 0.0;
	
	private Command					myCommands[];
	private	String					myScriptName;
	
	public OldServoChannelScript(ServoChannelConfig channelConfig, int numFrames, String scriptName) {
		myChannelConfig = channelConfig;
		myScriptName = scriptName;
		myCommands = new Command[numFrames];
		for (int i=0; i < numFrames; i++) {
			Command cmd = new Command();
			cmd.jointCommand = JointCommand.EMPTY;
			cmd.positionValue = -200.0;
			myCommands[i] = cmd;
		}
	}
	public OldServoChannelScript(OldServoChannelScript src) {
		myScriptName = src.myScriptName;
		myChannelConfig = src.myChannelConfig;
		myCommands = new Command[src.myCommands.length];
		for (int i=0; i < myCommands.length; i++) {
			myCommands[i] = new Command();
			myCommands[i].jointCommand = src.myCommands[i].jointCommand;
			myCommands[i].positionValue = src.myCommands[i].positionValue;
		}
	}

	public void absorbInstruction(int frameIndex, JointInstructionStruct jis) {
		Command cmd = myCommands[frameIndex];
		cmd.jointCommand = (JointCommand) jis.jointCommand.get();
		cmd.positionValue = jis.targetJointValue.get();
	}
	public int findNextNonemptyFrameIndex(int startingIndex) {
		int result = -1;
		for (int index = startingIndex; index < myCommands.length; index++) {
			if (myCommands[index].jointCommand != JointCommand.EMPTY) {
				result = index;
				break;
			}
		}
		return result;
	}
	// Goal is to glide from initial value onto the trajectory.
	// Complexity comes in as we try to fully account for possibly missing frames in the script
	public void adjustForInitialValue(double initialValue, int rampFrameCount, double mergeThreshold) {
		int totalFrameCount = myCommands.length;
		// If the ramp is longer than scripted animation, we'll just have to ramp harder.
		// We won't extend the animation.
		if (rampFrameCount >= totalFrameCount) {
			rampFrameCount = totalFrameCount;
		}
		int indexToUpdate = 0;
		int nextNonemptyFrameIndex = -1;
		double nextScriptedValue = -3333.33;
		while (indexToUpdate < rampFrameCount) {
			if (indexToUpdate > nextNonemptyFrameIndex) {
				nextNonemptyFrameIndex = findNextNonemptyFrameIndex(indexToUpdate);
			}
			if (nextNonemptyFrameIndex >= 0) {
				nextScriptedValue = myCommands[nextNonemptyFrameIndex].positionValue;
			} else {
				if (indexToUpdate == 0) {
					// This is a completely empty channel script, which is weird.
					theLogger.warning("Refusing to adjust empty channel script " + getDescription());
					return;
				} else {
					theLogger.warning("Continuing to adjust script ramp-up based on last " +
								" scripted value: " + nextScriptedValue + " in " + getDescription());
				}
			}
			double av = adjustedValue(initialValue, nextScriptedValue, indexToUpdate, rampFrameCount);
			myCommands[indexToUpdate].jointCommand = JointCommand.MOVE_ABSOLUTE_IMMEDIATE;
			myCommands[indexToUpdate].positionValue = av;
			// How far are we from scripted value for this frame?
			double deflection = Math.abs(av - nextScriptedValue);
			if (deflection < mergeThreshold) {
				// We're close enough to the scripted trajectory now.  Stop the adjustment process.
				theLogger.info("Channel deflection passed within merge threshold at frame " +
								indexToUpdate + " of " + getDescription() + " - truncating adjustment");
				break;
			}
			indexToUpdate++;
		}
	}
	private double adjustedValue(double startingValue, double scriptedValue, 
				int frameIndex, int rampTotalFrames) {
		double fraction = ((double) frameIndex) /  ((double) rampTotalFrames);
		double adjustedValue = scriptedValue * fraction + (1.0 - fraction) * startingValue;
		return adjustedValue;
	}
	public void writeToFrame(JointFrameStruct jfs, int frameIndex) {
		int logicalChannel = myChannelConfig.logicalChannel;
		Command cmd = myCommands[frameIndex];
		// if command is empty - copy previous value over?  or allow other rules (e.g. restoring
		// force) to move the servo somewhere else?
		jfs.addInstruction(logicalChannel, cmd.jointCommand, cmd.positionValue);
		// theLogger.finer("Wrote instruction, channel=" + logicalChannel + ", cmd=" + cmd.jointCommand + ", pos=" + cmd.positionValue);
	}
	public Command getCommandAtIndex (int frameIndex) {
		return myCommands[frameIndex];
	}
	public double getPositionValueAtIndex(int frameIndex) {
		// Beware!   if this is an "EMPTY" command, then the value is nonsense.
		return myCommands[frameIndex].positionValue;
	}
	public ServoChannelConfig getChannelConfig() {
		return myChannelConfig;
	}
	public String getDescription() {
		return myScriptName + "-logical channel=" +  myChannelConfig.logicalChannel;
	}

}
