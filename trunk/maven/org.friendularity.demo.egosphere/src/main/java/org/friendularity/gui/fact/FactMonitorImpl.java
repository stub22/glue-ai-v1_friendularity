/*
 *  Copyright 2008 Hanson Robotics Inc.
 *  All Rights Reserved.
 */

package org.friendularity.gui.fact;

import java.util.Observable;
import java.util.Observer;
import org.cogchar.zzz.oldboot.SubsystemImpl;
import org.cogchar.integroid.broker.IntegroidCueBroker;
import org.cogchar.integroid.broker.IntegroidJobBroker;
import org.cogchar.zzz.platform.stub.ThalamusBrokerStub;
import org.friendularity.app.jmxwrap.SignalStation;

/**
 *
 * @author Stu Baurmann
 */
public class FactMonitorImpl extends SubsystemImpl implements Observer {
	private	 boolean myInitializedFlag = false;
	private		IntegroidJobBroker		myIJB;
	private		IntegroidCueBroker		myICB;
	private		FactMonitorPanel		myPanel;
	public FactMonitorImpl(FactMonitorPanel fmp) {
		myPanel = fmp;
		SignalStation ss = SignalStation.getSignalStation();
		ss.addObserver(this);
	}
	public void ensureInitialized() {
		if (!myInitializedFlag) {
			/*
			ThalamusMonitorImpl tmi = (ThalamusMonitorImpl) lookupSubsystem(ThalamusMonitorImpl.class);
			if (tmi != null) {
				myIJB = tmi.getJobBroker();
				myICB = tmi.getCueBroker();
			}
			 * 
			 */
		}
	}
	public ThalamusBrokerStub getFactSourceBroker() { 
		ensureInitialized();
		return myICB;
	}
	public void update(Observable o, Object arg) {
		myPanel.refreshContents();
	}
}
