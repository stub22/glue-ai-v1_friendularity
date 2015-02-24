/**
 * Copyright 2008 Hanson Robotics Inc.
 * All Rights Reserved.
 */

package org.friendularity.gui.gaze;

import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;

import java.util.List;

/**
 *
 * @author humankind
 */
public class GazeTunerBean {
	public static final String PROP_AVAILABLE_GAZE_STRATEGY_LIST = "availableGazeStrategyList";
	public static final String PROP_GAZE_STRATEGY_NAME = "gazeStrategyName";
	public static final String PROP_CHANGE_GAZE_STRATEGY = "changeGazeStrategy";
	public static final String PROP_GAZE_FACE_NUMBER = "gazeFaceNumber";

	private PropertyChangeSupport propertyChangeSupport = new PropertyChangeSupport(this);

	
	private List			myAvailableGazeStrategyList = null;
	private String			myGazeStrategyName;
	
	private Integer			myGazeFaceNumber;


	public String getGazeStrategyName() {
        return myGazeStrategyName;
    }
    public void setGazeStrategyName(String name) {
        String oldName = myGazeStrategyName;
        myGazeStrategyName = name;
        propertyChangeSupport.firePropertyChange(PROP_GAZE_STRATEGY_NAME, oldName, name);
    }
	
	public Integer getGazeFaceNumber() {
        return myGazeFaceNumber;
    }
    public void setGazeFaceNumber(Integer num) {
        Integer oldNumber = myGazeFaceNumber;
        myGazeFaceNumber = num;
		/* This property change is picked up by the BlendingAnimator, and
		 * turned into a call to AnimoidFacade.suggestGazeSightNumber().
		 */
        propertyChangeSupport.firePropertyChange(PROP_GAZE_FACE_NUMBER, oldNumber, num);
    }	

	

	public List getAvailableGazeStrategyList() {
		return myAvailableGazeStrategyList;
	}

	public void setAvailableGazeStrategyList(List nameList) {
		List oldNameList = myAvailableGazeStrategyList;
		myAvailableGazeStrategyList = nameList;
		propertyChangeSupport.firePropertyChange(PROP_AVAILABLE_GAZE_STRATEGY_LIST, oldNameList, myAvailableGazeStrategyList);
	}	

	
	
	public void addPropertyChangeListener(PropertyChangeListener listener) {
		propertyChangeSupport.addPropertyChangeListener(listener);
	}

	/**
	 * Remove PropertyChangeListener.
	 *
	 * @param listener
	 */
	public void removePropertyChangeListener(PropertyChangeListener listener) {
		propertyChangeSupport.removePropertyChangeListener(listener);
	}
	
}
