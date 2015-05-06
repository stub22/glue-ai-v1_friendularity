/**
 * Copyright 2008 Hanson Robotics Inc.
 * All Rights Reserved.
 */

package org.friendularity.gui.blending;

import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;

import java.util.List;

/**
 *
 * @author humankind
 */
public class BlendingMonitorBean {
	public static final String PROP_USING_ANY_FACE_ATTENTION_RULE = "usingAnyFaceAttentionRule";
	public static final String PROP_USING_JOYSTICK_GAZE_CONTROL = "usingJoystickGazeControl";
	public static final String PROP_USING_VISEME_RULE = "usingVisemeRule";		
	public static final String PROP_USING_DEFAULT_RULE = "usingDefaultRule";	
	public static final String PROP_USING_BLINK_RULE = "usingBlinkRule";		
	public static final String PROP_USING_BROW_BLEND_RULE = "usingBrowBlendRule";
	public static final String PROP_USING_SCRIPTED_ANIMS = "usingScriptedAnimations";
	
	public static final String PROP_AVAILABLE_ANIMATION_LIST = "availableAnimationList";
	public static final String PROP_TARGET_ANIMATION_NAME = "targetAnimationName";
	public static final String PROP_AVAILABLE_GAZE_STRATEGY_LIST = "availableGazeStrategyList";
	public static final String PROP_GAZE_STRATEGY_NAME = "gazeStrategyName";
	public static final String PROP_CHANGE_GAZE_STRATEGY = "changeGazeStrategy";
	public static final String PROP_GAZE_FACE_NUMBER = "gazeFaceNumber";

	
	private PropertyChangeSupport propertyChangeSupport = new PropertyChangeSupport(this);

	protected boolean myUsingAnyFaceAttentionRule = false;
	protected boolean myUsingJoystickGazeControl = false;
	protected boolean myUsingVisemeRule = false;
	protected boolean myUsingDefaultRule = false;
	protected boolean myUsingBlinkRule = false;
	protected boolean myUsingBrowBlendRule = false;
	protected boolean myUsingScriptedAnimations = false;
	
	private List			myAvailableAnimationList = null;
    private String			myTargetAnimationName;
	
	private List			myAvailableGazeStrategyList = null;
	private String			myGazeStrategyName;
	
	private Integer			myGazeFaceNumber;

	
	public boolean isUsingAnyFaceAttentionRule() {
		return myUsingAnyFaceAttentionRule;
	}

	public void setUsingAnyFaceAttentionRule(boolean uafar) {
		boolean oldUsingAnyFaceAttentionRule = myUsingAnyFaceAttentionRule;
		myUsingAnyFaceAttentionRule = uafar;
		propertyChangeSupport.firePropertyChange(PROP_USING_ANY_FACE_ATTENTION_RULE, 
				oldUsingAnyFaceAttentionRule, myUsingAnyFaceAttentionRule);
	}
	public boolean isUsingBlinkRule() {
		return myUsingBlinkRule;
	}
	public void setUsingBlinkRule(boolean useIt) {
		boolean oldFlag = myUsingBlinkRule;
		myUsingBlinkRule = useIt;
		propertyChangeSupport.firePropertyChange(PROP_USING_BLINK_RULE, oldFlag, useIt);
	}
	public boolean isUsingBrowBlendRule() {
		return myUsingBrowBlendRule;
	}
	public void setUsingBrowBlendRule(boolean useIt) {
		boolean oldFlag = myUsingBrowBlendRule;
		myUsingBrowBlendRule = useIt;
		propertyChangeSupport.firePropertyChange(PROP_USING_BROW_BLEND_RULE, oldFlag, useIt);
	}
	
	
	public boolean isUsingDefaultRule() {
		return myUsingDefaultRule;
	}
	public void setUsingDefaultRule(boolean useIt) {
		boolean oldFlag = myUsingDefaultRule;
		myUsingDefaultRule = useIt;
		propertyChangeSupport.firePropertyChange(PROP_USING_DEFAULT_RULE, oldFlag, useIt);
	}
	
	
	public boolean isUsingVisemeRule() {
		return myUsingVisemeRule;
	}
	public void setUsingVisemeRule(boolean useIt) {
		boolean oldFlag = myUsingVisemeRule;
		myUsingVisemeRule = useIt;
		propertyChangeSupport.firePropertyChange(PROP_USING_VISEME_RULE, oldFlag, useIt);
	}

	public boolean isUsingScriptedAnimations() {
		return myUsingScriptedAnimations;
	}
	public void setUsingScriptedAnimations(boolean useIt) {
		boolean oldFlag = myUsingScriptedAnimations;
		myUsingScriptedAnimations = useIt;
		propertyChangeSupport.firePropertyChange(PROP_USING_SCRIPTED_ANIMS, oldFlag, useIt);
	}


	public boolean isUsingJoystickGazeControl() {
		return myUsingJoystickGazeControl;
	}

	public void setUsingJoystickGazeControl(boolean ujgc) {
		boolean oldUsingJoystickGazeControl = myUsingJoystickGazeControl;
		myUsingJoystickGazeControl = ujgc;
		propertyChangeSupport.firePropertyChange(PROP_USING_JOYSTICK_GAZE_CONTROL, 
					oldUsingJoystickGazeControl, myUsingJoystickGazeControl);
	}
	
    public String getTargetAnimationName() {
        return myTargetAnimationName;
    }

    public void setTargetAnimationName(String name) {
        String oldName = myTargetAnimationName;
        myTargetAnimationName = name;
        propertyChangeSupport.firePropertyChange(PROP_TARGET_ANIMATION_NAME, oldName, name);
    }
	
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

	
	public List getAvailableAnimationList() {
		return myAvailableAnimationList;
	}

	public void setAvailableAnimationList(List nameList) {
		List oldNameList = myAvailableAnimationList;
		myAvailableAnimationList = nameList;
		propertyChangeSupport.firePropertyChange(PROP_AVAILABLE_ANIMATION_LIST, oldNameList, myAvailableAnimationList);
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
