/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.friendularity.gui.egosphere;

import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.io.Serializable;
import java.util.logging.Logger;

/**
 *
 * @author Matthew Stevenson
 */
public class EgosphereMonitorBean extends Object implements Serializable {

    private PropertyChangeSupport myPCS;
    
    private boolean myDisplayVideo = false;
	

	public static final String PROP_DISPLAY_VIDEO = "displayVideo";

    public EgosphereMonitorBean() {
        myPCS = new PropertyChangeSupport(this);
    }

    public void addPropertyChangeListener(PropertyChangeListener listener) {
        myPCS.addPropertyChangeListener(listener);
    }

    public void removePropertyChangeListener(PropertyChangeListener listener) {
        myPCS.removePropertyChangeListener(listener);
    }

    public boolean isDisplayVideo() {
        return myDisplayVideo;
    }

    public void setDisplayVideo(boolean disp) {
        boolean oldDisp = myDisplayVideo;
        myDisplayVideo = disp;
        Logger.global.info("Bean is changing display video from " + oldDisp + " to " + myDisplayVideo);
        myPCS.firePropertyChange(PROP_DISPLAY_VIDEO, oldDisp, myDisplayVideo);        
    }
}
