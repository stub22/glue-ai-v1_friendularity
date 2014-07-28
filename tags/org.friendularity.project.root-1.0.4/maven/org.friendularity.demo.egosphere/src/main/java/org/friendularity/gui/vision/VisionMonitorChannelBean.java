/**
 * Copyright 2008 Hanson Robotics Inc.
 * All Rights Reserved.
 */

package org.friendularity.gui.vision;

import java.beans.*;
import java.io.Serializable;

import java.util.logging.Logger;

/**
 * @author Stu Baurmann
 */
public class VisionMonitorChannelBean extends Object implements Serializable {

    private PropertyChangeSupport myPCS;
    
    private int		myChannelNumber;
    private boolean myGrabbingFrames = false;
	private boolean myDetectingFaces = false;
	private boolean myDetectingMotion = false;
	private boolean myTrackingFaces = false;
	private boolean myRecognizingFaces = false;
	private boolean myFlippingVideoVertical = true;
	

	public static final String PROP_CHANNEL_NUMBER = "channelNumber";
	public static final String PROP_FLIPPING_VIDEO_VERTICAL = "flippingVideoVertical";
	public static final String PROP_GRABBING_FRAMES = "grabbingFrames";	
	public static final String PROP_DETECTING_FACES = "detectingFaces";
	public static final String PROP_DETECTING_MOTION = "detectingMotion";
	public static final String PROP_TRACKING_FACES = "trackingFaces";
	public static final String PROP_RECOGNIZING_FACES = "recognizingFaces";	


    public VisionMonitorChannelBean() {
        myPCS = new PropertyChangeSupport(this);
        myChannelNumber = -1;
    }

    public void addPropertyChangeListener(PropertyChangeListener listener) {
        myPCS.addPropertyChangeListener(listener);
    }

    public void removePropertyChangeListener(PropertyChangeListener listener) {
        myPCS.removePropertyChangeListener(listener);
    }

    public int getChannelNumber() {
        Logger.global.severe("Bean is getting channel: " + myChannelNumber);
        return myChannelNumber;
    }

    public void setChannelNumber(int channelNum) {
        int oldCN = myChannelNumber;
        myChannelNumber = channelNum;
        Logger.global.info("Bean is changing channel from " + oldCN + " to " + myChannelNumber);
        myPCS.firePropertyChange(PROP_CHANNEL_NUMBER, oldCN, myChannelNumber);        
    }
    /**
     * Get the value of grabbingFrames
     *
     * @return the value of grabbingFrames
     */
    public boolean isGrabbingFrames() {
        return myGrabbingFrames;
    }

    /**
     * Set the value of grabbingFrames
     *
     * @param grabbingFrames new value of grabbingFrames
     */
    public void setGrabbingFrames(boolean gf) {
        boolean oldGF = myGrabbingFrames;
        myGrabbingFrames = gf;
        Logger.global.finer("grabbingFrames is now : " + myGrabbingFrames);
        myPCS.firePropertyChange(PROP_GRABBING_FRAMES, oldGF, myGrabbingFrames);
    }

	/**
	 * Get the value of trackingFaces
	 *
	 * @return the value of trackingFaces
	 */
	public boolean isTrackingFaces() {
		return myTrackingFaces;
	}

	/**
	 * Set the value of trackingFaces
	 *
	 * @param trackingFaces new value of trackingFaces
	 */
	public void setTrackingFaces(boolean tf) {
		boolean oldTF = myTrackingFaces;
		myTrackingFaces = tf;
		myPCS.firePropertyChange(PROP_TRACKING_FACES, oldTF, myTrackingFaces);
	}

	/**
	 * Get the value of detecingFaces
	 *
	 * @return the value of detecingFaces
	 */
	public boolean isDetectingFaces() {
		return myDetectingFaces;
	}

	/**
	 * Set the value of detecingFaces
	 *
	 * @param detecingFaces new value of detecingFaces
	 */
	public void setDetectingFaces(boolean df) {
		boolean oldDF = myDetectingFaces;
		myDetectingFaces = df;
		myPCS.firePropertyChange(PROP_DETECTING_FACES, oldDF, myDetectingFaces);
	}

	/**
	 * Get the value of detectingMotion
	 *
	 * @return the value of detectingMotion
	 */
	public boolean isDetectingMotion() {
		return myDetectingMotion;
	}

	/**
	 * Set the value of detectingMotion
	 *
	 * @param detectingMotion new value of detectingMotion
	 */
	public void setDetectingMotion(boolean dm) {
		boolean oldDM = myDetectingMotion;
		myDetectingMotion = dm;
		myPCS.firePropertyChange(PROP_DETECTING_MOTION, oldDM, myDetectingMotion);
	}


	/**
	 * Get the value of recognizingFaces
	 *
	 * @return the value of recognizingFaces
	 */
	public boolean isRecognizingFaces() {
		return myRecognizingFaces;
	}

	/**
	 * Set the value of recognizingFaces
	 *
	 * @param recognizingFaces new value of recognizingFaces
	 */
	public void setRecognizingFaces(boolean rf) {
		boolean oldRF = myRecognizingFaces;
		myRecognizingFaces = rf;
		myPCS.firePropertyChange(PROP_RECOGNIZING_FACES, oldRF, myRecognizingFaces);
	}

	/**
	 * Get the value of invertVideo
	 *
	 * @return the value of invertVideo
	 */
	public boolean isFlippingVideoVertical() {
		return myFlippingVideoVertical;
	}

	/**
	 * Set the value of invertVideo
	 *
	 * @param invertVideo new value of invertVideo
	 */
	public void setFlippingVideoVertical(boolean fvv) {
		boolean oldFVV = myFlippingVideoVertical;
		myFlippingVideoVertical = fvv;
		myPCS.firePropertyChange(PROP_FLIPPING_VIDEO_VERTICAL, oldFVV, myFlippingVideoVertical);
	}

}
