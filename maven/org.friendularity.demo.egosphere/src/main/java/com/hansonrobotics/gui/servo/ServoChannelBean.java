/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package com.hansonrobotics.gui.servo;


import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.io.Serializable;
import org.cogchar.animoid.config.ServoChannelConfig;

/**
 * @author Stu Baurmann
 */
public class ServoChannelBean extends Object implements Serializable {
	private PropertyChangeSupport myPropertySupport;
	
	protected			ServoChannelConfig		myServoChannelConfig;
	protected			double					myLogicalJointValue;
	protected			double					myAbsRomPos;
	
	// public static final String PROP_JOINT_NAME = "jointName";
	// public static final String PROP_LOGICAL_JOINT_ID = "logicalJointID";
	public static final String PROP_LOGICAL_JOINT_VALUE = "logicalJointValue";
	public static final String PROP_ABS_ROM_POS = "absRomPos";

    public ServoChannelBean(ServoChannelConfig scc) {
		myServoChannelConfig = scc;
        myPropertySupport = new PropertyChangeSupport(this);
    }
	public double getLogicalJointValue() {
		return myLogicalJointValue;
	}
	public void setLogicalJointValue(double ljv) {
		double oldLJV = myLogicalJointValue;
		myLogicalJointValue = ljv;
		myPropertySupport.firePropertyChange(PROP_LOGICAL_JOINT_VALUE, oldLJV, myLogicalJointValue);
	}
	public double getAbsRomPos() {
		return myAbsRomPos;
	}
	public void setAbsRomPos(double arp) {
		double oldARP = myAbsRomPos;
		myAbsRomPos = arp;
		myPropertySupport.firePropertyChange(PROP_ABS_ROM_POS, oldARP, myAbsRomPos);
	}
	public int getLogicalJointID() {
		return myServoChannelConfig.logicalChannel;
	}
	public String getJointName() {
		return myServoChannelConfig.getMuscleJoint().name();
	}

    public void addPropertyChangeListener(PropertyChangeListener listener) {
        myPropertySupport.addPropertyChangeListener(listener);
    }

    public void removePropertyChangeListener(PropertyChangeListener listener) {
        myPropertySupport.removePropertyChangeListener(listener);
    }

}

/*
	public void setLogicalJointID(int logicalJointID) {
		int oldLogicalJointID = logicalJointID;
		this.logicalJointID = logicalJointID;
		propertySupport.firePropertyChange(PROP_LOGICAL_JOINT_ID, oldLogicalJointID, logicalJointID);
	}

	public void setJointName(String jointName) {
		String oldJointName = jointName;
		this.jointName = jointName;
		propertySupport.firePropertyChange(PROP_JOINT_NAME, oldJointName, jointName);
	}
	 */
 
