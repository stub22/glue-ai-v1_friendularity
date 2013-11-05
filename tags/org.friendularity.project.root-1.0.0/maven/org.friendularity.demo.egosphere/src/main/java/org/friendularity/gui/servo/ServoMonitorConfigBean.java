/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.friendularity.gui.servo;

import java.beans.*;
import java.io.Serializable;

/**
 * @author Stu Baurmann
 */
public class ServoMonitorConfigBean extends Object implements Serializable {

	private PropertyChangeSupport	myPropertySupport;

    public static final String PROP_MONITORING = "monitoring";

    private boolean		myMonitoringFlag = false;

    public ServoMonitorConfigBean() {
        myPropertySupport = new PropertyChangeSupport(this);
    }

    public boolean isMonitoring() {
		return myMonitoringFlag;
    }

    public void setMonitoring(boolean mflag) {
		boolean oldMF = myMonitoringFlag;
		myMonitoringFlag = mflag;
        myPropertySupport.firePropertyChange(PROP_MONITORING, oldMF, myMonitoringFlag);
    }

    public void addPropertyChangeListener(PropertyChangeListener listener) {
        myPropertySupport.addPropertyChangeListener(listener);
    }

    public void removePropertyChangeListener(PropertyChangeListener listener) {
        myPropertySupport.removePropertyChangeListener(listener);
    }

}
