/*
 *  Copyright 2008 Hanson Robotics Inc.
 *  All Rights Reserved.
 */

package org.friendularity.gui.servo;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.cogchar.animoid.monitor.IServoMonitor;
import org.cogchar.animoid.monitor.IServoPositionReporter;

import org.cogchar.api.animoid.config.bonus.ServoChannelConfig;
import org.cogchar.api.animoid.protocol.JPARFrame;
import org.cogchar.api.animoid.protocol.JointPosition;
import org.cogchar.api.animoid.protocol.JointPositionAROM;
import org.cogchar.api.animoid.protocol.JointPositionSnapshot;
import org.cogchar.api.animoid.protocol.JointStateCoordinateType;
import org.cogchar.zzz.oldboot.SubsystemImpl;
import org.friendularity.app.animation.BlendingAnimator;
import org.friendularity.gui.blending.BlendingMonitorImpl;
import org.jdesktop.observablecollections.ObservableCollections;
import org.jdesktop.observablecollections.ObservableList;

/**
 * @author Stu Baurmann
 */
public class ServoMonitorImpl extends SubsystemImpl implements IServoMonitor, PropertyChangeListener {
	private static Logger	theLogger = Logger.getLogger(ServoMonitorImpl.class.getName());
	
	private		ObservableList<ServoChannelBean>	myObservableServoChannelList;
	private		boolean								myInitializedFlag = false;	
	// private		BlendingAnimator					myBlendingAnimator;

	private		IServoPositionReporter				myPositionReporter;

	public ServoMonitorImpl() {
		List<ServoChannelBean>		innerSCB_List = new ArrayList<ServoChannelBean>();
		myObservableServoChannelList = ObservableCollections.observableList(innerSCB_List);
	}
	public void setConfigBean(ServoMonitorConfigBean smcb) {
		smcb.addPropertyChangeListener(this);
	}
	public void setServoPositionReporter(IServoPositionReporter rep) {
		myPositionReporter = rep;
	}
	public ObservableList<ServoChannelBean> getServoChannelList() {
		return myObservableServoChannelList;
	}
	public void ensureInitialized() {
		if (!myInitializedFlag) {
			try {
				BlendingMonitorImpl bmi = (BlendingMonitorImpl) lookupSubsystem(BlendingMonitorImpl.class); 
				BlendingAnimator ba = bmi.getBlendingAnimator();
				myPositionReporter = bmi.getServoPositionReporter();
				ServoChannelConfig[] sscSparseArray = ba.getAnimoidFacade().getServoChannelConfigSparseArray();

				for (int i=0; i< sscSparseArray.length; i++) {
					ServoChannelConfig ssc = sscSparseArray[i];
					if (ssc != null) {
						ServoChannelBean scb = new ServoChannelBean(ssc);
						myObservableServoChannelList.add(scb);
					}
				}
			} catch (Throwable t) {
				theLogger.log(Level.SEVERE, "Can't init ServoMonitorImpl", t);
			}
			myInitializedFlag = true;
		}
	}
	public void updateValues() {
		// This is currently done only in response to GUI refresh button-action.
		ensureInitialized();
		// myBlendingAnimator.getJointPositionSnapshot();
		JointPositionSnapshot jps = myPositionReporter.getServoSnapshotLopsided();
		// This value is not cached by blending animator...so this would
		// be wasteful if done frequently.
		JPARFrame jarf = myPositionReporter.getServoSnapshotAROM();
		servoSnapshotUpdate(jps, jarf);
	}
	public void servoSnapshotUpdate(JointPositionSnapshot lopsidedSnapshot, JPARFrame absRomSnapshot) {
		if (lopsidedSnapshot != null) {
			servoSnapshotUpdateLopsided(lopsidedSnapshot);
		}
		if (absRomSnapshot != null) {
			servoSnapshotUpdateAROM(absRomSnapshot, (lopsidedSnapshot == null));
		}
	}

	private void servoSnapshotUpdateLopsided(JointPositionSnapshot lopsidedSnapshot) { 
		if (lopsidedSnapshot != null) {
			for (ServoChannelBean scb : myObservableServoChannelList) {
				JointPosition lopsidedJP = lopsidedSnapshot.getJointPositionForOldLogicalJointNumber(scb.getLogicalJointID());
				double currentValue = lopsidedJP.getCoordinateFloat(JointStateCoordinateType.FLOAT_ABS_LOPSIDED_PIECEWISE_LINEAR);
				scb.setLogicalJointValue(currentValue);
			}
		} else {
			theLogger.log(Level.SEVERE, "Got a null Lopsided snapshot");
		}
	}
	private void servoSnapshotUpdateAROM(JPARFrame absRomSnapshot, boolean clearLopsided) {
		if (absRomSnapshot != null) {
			for (ServoChannelBean scb : myObservableServoChannelList) {
				JointPositionAROM absRomJP = absRomSnapshot.getJointPositionForOldLogicalJointNumber(scb.getLogicalJointID());
				double absRomPos = absRomJP.getPosAbsROM();
				scb.setAbsRomPos(absRomPos);
				if (clearLopsided) {
					scb.setLogicalJointValue(-99.9);
				}
			}
		} else {
			theLogger.log(Level.SEVERE, "Got a null AROM snapshot");
		}
	}

	private void startMonitoringServoValueChanges() {
		ensureInitialized();
		myPositionReporter.setMonitor(this);
	}
	private void stopMonitoringServoValueChanges() {
		ensureInitialized();
		myPositionReporter.setMonitor(null);
	}
	public void propertyChange(PropertyChangeEvent evt) {
		String propertyName = evt.getPropertyName();
		Object propertyValue = evt.getNewValue();
		theLogger.fine("ServoMonitorImpl got property change:  " + propertyName + " := " + propertyValue);
		if (java.beans.Beans.isDesignTime()) {
			theLogger.fine("It's design time!  No further processing of event");
			return;
		}
		ensureInitialized();
		if (propertyName.equals(ServoMonitorConfigBean.PROP_MONITORING)) {
            if (propertyValue.equals(Boolean.TRUE)) {
                theLogger.finer("monitoring property is now TRUE!");
				startMonitoringServoValueChanges();
			} else {
				stopMonitoringServoValueChanges();
			}
		}
	}
}
