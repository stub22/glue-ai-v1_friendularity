/*
 *  Copyright 2011 by The Cogchar Project (www.cogchar.org).
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
package org.friendularity.gaze.api;

import java.net.URL;
import java.util.HashMap;
import java.util.List;
import java.util.Map;


import org.friendularity.gaze.job.AttentionJob;



import org.cogchar.api.animoid.protocol.EgocentricDirection;
import org.cogchar.api.animoid.protocol.Frame;
import org.friendularity.gaze.estimate.GazeDirectionComputer;

import org.friendularity.gaze.util.GazeStrategyCue;
import org.freckler.sight.impl.hypo.AnimoidSightFacade;

import org.cogchar.api.animoid.gaze.IGazeTarget;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 *
 * @author Stu B. <www.texpedient.com>
 * 
 * Lower level components should NOT know about the AnimoidFacade.
 */
public class AnimoidGazeFacade extends AnimoidSightFacade {
	private static Logger	theLogger = LoggerFactory.getLogger(AnimoidGazeFacade.class.getName());

	// private		AnimoidConfig			myAnimoidConfig;	



	
	public AnimoidGazeFacade(String servoConfigPath, URL animoidConfigURL, 
				String visemeConfigPath, Integer msecPerFrame,
				double frameDurationSmoothingFactor) throws Throwable {
		super(servoConfigPath, animoidConfigURL, visemeConfigPath, msecPerFrame, frameDurationSmoothingFactor);
		

	}
	public AttentionJob getAttentionJob() { 
		/* BlenderJob useta do:
	//theTestAttentionJob = new AttentionJob(this, sm, aconf, jobSpace);
	//	jobSpace.postManualJob(theTestAttentionJob);
	* So then we could just:
	* return myBlenderJob.theTestAttentionJob;
		 */
		return null;
	}
	
	public AnimoidGazeConfig getAGC() { 
		return (AnimoidGazeConfig) myAnimoidConfig;
	}
	GazeStrategyCue getNamedGazeStrategy(String name) { 
		return null;  // GazeStrategyCue holdAndRecenterStrategy = myAnimoidConfig.getNamedGazeStrategy(holdStrategyName);
	}
	public void suggestGazeStrategyName(String gazeStrategyName) {
		GazeStrategyCue gazeStrategy = getNamedGazeStrategy(gazeStrategyName);
		getAttentionJob().suggestGazeStrategy(gazeStrategy);
	}
	public void suggestHoldStrategyName(String holdStrategyName) {
		GazeStrategyCue holdAndRecenterStrategy = getNamedGazeStrategy(holdStrategyName);
		getAttentionJob().suggestHoldAndRecenterStrategy(holdAndRecenterStrategy);
	}
	public void suggestAttentionTarget(IGazeTarget target) {
		// Please do not call this method.  It's not really public!!!
		// As of 2010-3-27, it should only be called from PersonResolver.
		getAttentionJob().suggestAttentionTarget(target);
	}
	public void suggestAttentionState(boolean state) {
		theLogger.warn("Requested gaze attention state: " + state);
	}	
	public IGazeTarget getAttentionTarget() {
		return getAttentionJob().getAttentionTarget();
	}

	public GazeDirectionComputer getGazeDirectionComputer() { 
		return (GazeDirectionComputer) getSightModel().getGazeDirectionComputer();
	}
	public EgocentricDirection getCurrentEgocentricDirection(){
		Frame f = myBlenderJob.estimatePositionNow(true);
		if (f != null) {
			return getGazeDirectionComputer().computeGazeCenterDirection(f);
		} else {
			return null;
		}
	}

	public List<GazeJoint> getAllGazeJoints() {
		return getAGC().getGazeJoints();
	}

	public void rebalanceGazeJobs() {
		AttentionJob aj = getAttentionJob();
		if (aj != null) {
			aj.rebalanceGazeJobs();
		}
	}
	public void enableAttentionGaze() {
		theLogger.info("Enabling attention gaze");
		AttentionJob aj = getAttentionJob();
		if (aj != null) {
			aj.enableMotion();
		}
		myCueSpace.clearMatchingNamedCues(T_ATTENTION_DISABLED);
		myCueSpace.addThoughtCueForName(T_ATTENTION_ENABLED, 1.0);
	}
	public void disableAttentionGaze() {
		theLogger.info("Disabling attention gaze");
		AttentionJob aj = getAttentionJob();
		if (aj != null) {
			aj.disableMotion();
		}
		myCueSpace.clearMatchingNamedCues(T_ATTENTION_ENABLED);
		myCueSpace.addThoughtCueForName(T_ATTENTION_DISABLED, 1.0);
	}

	public String getAttentionDebugText() {
		String attentionDebugText = "No Attention Job";
		AttentionJob aj = getAttentionJob();
		if (aj != null) {
			attentionDebugText = aj.getAttentionDebugText();
		}
		EgocentricDirection currDir = getCurrentEgocentricDirection();
		String debugText ="currDir=" + currDir + "\n\n" + attentionDebugText;
		return debugText;
	}
		
	/*
	public JointPositionSnapshot getCurrentGazeSnapshot(){
		Frame thisFrame = myBlenderJob.estimatePositionNow(true);
		// Note - this uses hardcoded muscle joint IDs!
		return JointPositionSnapshot.getGazeSnapshot(thisFrame);
	}
	 */
	/*
	public Boolean getGazeHoldStatus() {
		AttentionJob aj = myBlenderJob.theTestAttentionJob;
		if (aj != null) {
			return aj.getHoldingStatusFlag();
		} else {
			return null;
		}
	}
	 */
	/*
	public double getEgocentricXAbsRoM(){
		ensureEgoExtremes();
		Double range = myMaxEgoX - myMinEgoX;
		Double curAz = getCurrentEgocentricDirection().getAzimuth().getDegrees();
		curAz -= myMinEgoX;
		return curAz/range;
	}
	public double getEgocentricYAbsRoM(){
		ensureEgoExtremes();
		Double range = myMaxEgoY - myMinEgoY;
		Double curEl = getCurrentEgocentricDirection().getElevation().getDegrees();
		curEl -= myMinEgoY;
		return curEl/range;
	}
	private void ensureEgoExtremes(){
		if(myMinEgoX != null && myMaxEgoX != null &&
				myMinEgoY != null && myMaxEgoY != null){
			return;
		}
		Frame center = AnimationBuilder.makeGazeCenteringFrame(getAllGazeJoints());
		Frame minF = getJointPositionFrame(center, 0.0);
		Frame maxF = getJointPositionFrame(center, 1.0);

		GazeDirectionComputer gdc = mySightModel.getGazeDirectionComputer();
		if(gdc == null){
			theLogger.fine("Gaze Direction Computer is null, cannot continue.");
			return;
		}
		EgocentricDirection edMax = gdc.computeGazeCenterDirection(maxF);
		EgocentricDirection edMin = gdc.computeGazeCenterDirection(minF);
		if(edMax == null || edMin == null){
			theLogger.fine("Egocentric Direction is null, cannot continue.");
			return;
		}
		myMinEgoX = edMin.getAzimuth().getDegrees();
		myMaxEgoX = edMax.getAzimuth().getDegrees();
		myMinEgoY = edMin.getElevation().getDegrees();
		myMaxEgoY = edMax.getElevation().getDegrees();
	}
*/
		/*
	public Animation convertDenseAbsROMtoDenseRelROM(Animation absAnim) {
		Animation relAnim = new Animation(absAnim.getName(), absAnim.getFramePeriodSeconds());
		int frameCount = absAnim.getFrameCount();
		// First rel frame will always be all 0.0's
		Frame prevAbsFrame = absAnim.getFrameAt(0);
		for(int frameIDX=0; frameIDX < frameCount; frameIDX++) {
			Frame nextAbsFrame = absAnim.getFrameAt(frameIDX);
			Frame nextRelFrame = new Frame();
			List<JointPosition> nextAbsJPs = nextAbsFrame.getAllPositions();
			for (JointPosition nextAbsJP : nextAbsJPs) {
				Joint j = nextAbsJP.getJoint();
				JointPosition prevAbsJP = prevAbsFrame.getJointPositionForJoint(j);
				double nextAbsPos = nextAbsJP.getCoordinateFloat(JointPosition.CoordinateType.FLOAT_ABS_RANGE_OF_MOTION);
				double prevAbsPos = prevAbsJP.getCoordinateFloat(JointPosition.CoordinateType.FLOAT_ABS_RANGE_OF_MOTION);
				double moveDelta = nextAbsPos - prevAbsPos;
				JointPosition nextRelJP = new JointPosition(j);
				nextRelJP.setCoordinateFloat(JointPosition.CoordinateType.FLOAT_REL_RANGE_OF_MOTION, moveDelta);
				nextRelFrame.addPosition(nextRelJP);
			}
			relAnim.appendFrame(nextRelFrame);
			prevAbsFrame = nextAbsFrame;
		}
		return relAnim;
	}
	*/
	/*
	public void initGazeReturnAnim(){
		//using "INIT" as a temporary placeholder.  Works fine now, but we will
		//need to make many animations on the fly.  This is a temporary method
		initGazeReturnAnim("INIT");
	}
	public void initGazeReturnAnim(String init){
		Animation a = new Animation(init, 1.0);
		Frame snapshot = getCurrentGazeSnapshot();
		a.appendFrame(snapshot);
		myAnimationLibrary.registerAnimation(a);
	}
	public void completeGazeReturnAnim(String name, int length){
		//same as initGazeReturnAnim
		completeGazeReturnAnim("INIT", name, length);
	}
	public void completeGazeReturnAnim(String init, String name, int length){
		//same as initGazeReturnAnim
		Animation base = myAnimationLibrary.getAnimationForName(init);
		Frame start = getCurrentGazeSnapshot();
		Frame end = base.getFrameAt(0);
		Animation a = AnimationBuilder.makeLinearAnimation(name, start, end, length);
		myAnimationLibrary.registerAnimation(a);
	}
	public void makeCenteringAnimation(){
		Frame start = getCurrentGazeSnapshot();
		Frame end = AnimationBuilder.makeGazeCenteringFrame(getAllGazeJoints());
		Animation complete = AnimationBuilder.makeLinearAnimation("Centering", start, end, 10);
		myAnimationLibrary.registerAnimation(complete);
	}
    public void makeNeckHoldAnimation(int frameCount){
        List<JointPosition> pos = new ArrayList();
        Frame<JointPosition> f = myBlenderJob.estimatePositionNow(true);
        for(GazeJoint j : getAllGazeJoints()){
            pos.add(f.getJointPositionForJoint(j.getJoint()));
        }
        Animation a = AnimationBuilder.makeConstantAnimation("HOLD_NECK", frameCount, pos, 0.05);
        myAnimationLibrary.registerAnimation(a);
    }
	 */
	/*
	private Frame getJointPositionFrame(Frame center, double val) {
		Frame f = new Frame();
		List<JointPosition> jps = center.getAllPositions();
		for (JointPosition jp : jps) {
			JointPosition newJp = jp.convertToCooordinateType(JointStateCoordinateType.FLOAT_ABS_RANGE_OF_MOTION);
			newJp.setCoordinateFloat(JointStateCoordinateType.FLOAT_ABS_RANGE_OF_MOTION, val);
			f.addPosition(newJp);
		}
		return f;
	}
	*/

}
