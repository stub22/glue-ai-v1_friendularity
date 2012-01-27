/*
 *  Copyright 2008 Hanson Robotics Inc.
 *  All Rights Reserved.
 */

package org.friendularity.app.animation;

import java.util.Set;
import java.util.logging.Logger;
import org.cogchar.animoid.job.AnimationExecJob;
import org.cogchar.api.animoid.protocol.Frame;
import org.cogchar.api.animoid.protocol.Joint;

/**
 *
 * @author Stu Baurmann
 */
public class OldExecutingAnimation {
	private static Logger	theLogger = Logger.getLogger("com.hansonrobotics.app.animation.ExecutingAnimation");
	
	private	OldAnimationScript			myScript;
//	private int						myFrameCount;
//	private	int						myNextFrameToPlay;
	
	// private	ServoChannelScript[]	myAdjustableChannelScripts;
	// private	boolean					myScriptsAreAdjusted;
	// Hacking this in here as scaffolding, it will eventually replace the OldExecutingAnimation.
	private	AnimationExecJob		myAnimationExecJob;
	// More hacking
	// private BlendingAnimator		myCrutchBA;
	
	public OldExecutingAnimation(OldAnimationScript script, BlendingAnimator crutchBA) {
	//	myCrutchBA = crutchBA;
		myScript = script;
	//	myFrameCount = myScript.getFrameCount();
	//	int channelCount = script.getChannelCount();
	//	myAdjustableChannelScripts = new ServoChannelScript[channelCount];
		// makeAdjustableChannelScripts();
		// reset();
	}
	public void setAnimationExecJob(AnimationExecJob aej) {
		myAnimationExecJob = aej;
	}
	public AnimationExecJob getJob() {
		return myAnimationExecJob;
	}
	/* We aint adjusting this way any more.
	private void makeAdjustableChannelScripts() {
		int idx = 0;
		for (ServoChannelScript scs : myScript.getChannelScripts()) {
			ServoChannelScript adjustable = new ServoChannelScript(scs);
			myAdjustableChannelScripts[idx++] = adjustable;
		}
		myScriptsAreAdjusted = false;
	}
	 */
	/* Olde absolute anim ramping technique - replaced by velocity calcs in 
	 * AnimationExecJob
	 *
	// Big assumption is that jfs contains ALL positions we need to adjust for.
	public void adjustForCurrentPositions(JointFrameStruct jfs, int rampFrameCount, double mergeThreshold) {
		if (myScriptsAreAdjusted) {
			makeAdjustableChannelScripts();
		}
		myScriptsAreAdjusted = true;
		Map<Integer,JointInstructionStruct> initialPositions = jfs.getNonemptyInstructionsByLogicalJoint();
		for (int c=0; c < myAdjustableChannelScripts.length; c++) {
			ServoChannelScript adjustable = myAdjustableChannelScripts[c];
			int adjustmentChannel = adjustable.getChannelConfig().logicalChannel;
			JointInstructionStruct initialPosInstruct = initialPositions.get(adjustmentChannel);
			if (initialPosInstruct != null) {
				JointCommand adjustmentCommand = (JointCommand) initialPosInstruct.jointCommand.get();
				if (adjustmentCommand != JointCommand.MOVE_ABSOLUTE_IMMEDIATE) {
					theLogger.warning("Adjustment frame has unexpected instruction command value: " 
								+ adjustmentCommand + " in executing animation " + getDescription());
					continue;
				}
				double initialValue = initialPosInstruct.targetJointValue.get();
				adjustable.adjustForInitialValue(initialValue, rampFrameCount, mergeThreshold);
			} else {
				theLogger.finer("No adjustment instruction found for logical channel: " + adjustmentChannel);
			}
		}
	}

	public void reset() {
		myNextFrameToPlay = 0;
	}
	 */
	public boolean hasMoreFrames() {
		// return (myNextFrameToPlay  < myFrameCount);
		return myAnimationExecJob.hasMoreFrames();
	}

	// public void playFrame(JointFrameStruct jfs) {
	public void playFrameIntoTarget(Frame targetFrame) {	
		Frame animFrame = myAnimationExecJob.takeFrameAndAdvance();

		Set<Joint> jointMask = animFrame.getUsedJointSet();
		boolean okToExtendDimensions = false;
	
		animFrame.transformOtherFrame(targetFrame, jointMask, false);
		
		/*  Aulde waye:
		for (int c=0; c < myAdjustableChannelScripts.length; c++) {
			myAdjustableChannelScripts[c].writeToFrame(jfs, myNextFrameToPlay);
		}
		advanceFrame();
		 */
	}
	
	/*
	public void advanceFrame() {
		myNextFrameToPlay++;
	}
	*/
	public String getScriptName() {
		return myScript.getName();
	}
	public String getDescription() {
		return "ExecutingAnimation based on " + getScriptName();
	}
}
