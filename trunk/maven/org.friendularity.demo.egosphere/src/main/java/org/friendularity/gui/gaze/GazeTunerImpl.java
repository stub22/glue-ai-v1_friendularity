/**
 * Copyright 2008 Hanson Robotics Inc.
 * All Rights Reserved.
 */

package org.friendularity.gui.gaze;


import org.friendularity.app.animation.BlendingAnimator;
import org.friendularity.gui.blending.BlendingMonitorImpl;


import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeEvent;

import java.util.logging.Logger;


import org.cogchar.animoid.broker.AnimoidFacade;
import org.cogchar.animoid.gaze.GazeStrategyCue;
import org.cogchar.integroid.boot.SubsystemImpl;
import org.cogchar.integroid.broker.IntegroidCueBroker;

/**
 *
 * @author Stu Baurmann
 */
public class GazeTunerImpl extends SubsystemImpl implements PropertyChangeListener {
	private static Logger	theLogger = Logger.getLogger(GazeTunerImpl.class.getName());

	private	GazeTunerBean		myConfigBean = null;

	private	BlendingAnimator	myBlendingAnimator;
	
	public GazeTunerImpl() {
	}
	
	public void setConfigBean(GazeTunerBean gtb) {
		theLogger.fine("AMI.setConfigBean()");
		myConfigBean = gtb;
		myConfigBean.addPropertyChangeListener(this);
	}
	private synchronized boolean ensureInitialized() {
		if (myBlendingAnimator == null) {
			BlendingMonitorImpl bmi = (BlendingMonitorImpl) lookupSubsystem(BlendingMonitorImpl.class);
			myBlendingAnimator = bmi.getBlendingAnimator();
		}
		return (myBlendingAnimator != null);
	}
	public BlendingAnimator getBlendingAnimator() {
		ensureInitialized();
		return myBlendingAnimator;
	}
	public AnimoidFacade getAnimoidFacade() {
		BlendingAnimator bm = getBlendingAnimator();
		if (bm != null) {
			return bm.getAnimoidFacade();
		} else {
			theLogger.warning("BlendingAnimator is null, cannot fetch AnimoidFacade!");
			return null;
		}
	}
    public void propertyChange(PropertyChangeEvent evt) {
		String propertyName = evt.getPropertyName();
		Object propertyValue = evt.getNewValue();
		theLogger.fine("BlendingMonitorImpl got property change:  " + propertyName + " := " + propertyValue);
		if (java.beans.Beans.isDesignTime()) {
			theLogger.fine("It's design time!  No further processing of event");
			return;
		}
		ensureInitialized();
		AnimoidFacade	af = getAnimoidFacade();
	}
	/*
	public void startUsingAnyFaceAttentionRule() {
		AnimoidFacade af = getAnimoidFacade();
		if (af != null) {
			af.enableAttentionGaze();
		}
	}
	public void stopUsingAnyFaceAttentionRule() {
		AnimoidFacade af = getAnimoidFacade();
		if (af != null) {
			af.disableAttentionGaze();
		}
	}
	*/

	public void changeGaze(String name, Integer refresh, Double slackX,
			Double slackY, Double flatJump, Double jumpRatio, 
			Double brakePower, Double brakeSlope){
		myBlendingAnimator.getAnimoidFacade().suggestGazeStrategyName("noGaze");
		GazeStrategyCue oldGsc = getGaze(name);
		oldGsc.setRefreshPeriodFrames(refresh);
		oldGsc.setSlackHorizDeg(slackX);
		oldGsc.setSlackVertDeg(slackY);
		oldGsc.setFlatJumpSize(flatJump);
		oldGsc.setDistanceJumpRatio(jumpRatio);
		oldGsc.setBrakePower(brakePower);
		oldGsc.setBrakeSlope(brakeSlope);
		myBlendingAnimator.getAnimoidFacade().suggestGazeStrategyName(name);
	}
	public GazeStrategyCue getGaze(String name){
		return myBlendingAnimator.getAnimoidFacade().
				getAnimoidConfig().getNamedGazeStrategy(name);
	}
	public IntegroidCueBroker getCueBroker(){
		try{
			return null;
			// return ((ThalamusMonitorImpl)lookupSubsystem(ThalamusMonitorImpl.class)).getCueBroker();
		} catch (Exception e){}
		return null;
	}
	protected void logWarning(String msg) {
		theLogger.warning(msg);
	}

}