/**
 * Copyright 2008 Hanson Robotics Inc.
 * All Rights Reserved.
 */

package org.friendularity.gui.blending;

import org.friendularity.app.animation.BlendingAnimator;
import org.friendularity.app.animation.MotionControlProxy;
import org.friendularity.nwrap.blend.InternalBlendingFacade;
import org.friendularity.nwrap.control.ServoControlFacade;

import org.friendularity.gui.vision.VisionMonitorChannelImpl;

import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeEvent;

import java.util.logging.Logger;

import java.util.logging.Level;

import org.cogchar.ancient.utility.Parameters;
import org.cogchar.animoid.broker.AnimoidFacade;
import org.cogchar.animoid.broker.MotionController;
import org.cogchar.animoid.protocol.IServoPositionReporter;
import org.cogchar.integroid.boot.ConfigSystemImpl;
import org.cogchar.integroid.boot.SubsystemImpl;


/**
 *
 * @author Stu Baurmann
 */
public class BlendingMonitorImpl extends SubsystemImpl implements PropertyChangeListener {
	private static Logger	theLogger = Logger.getLogger(BlendingMonitorImpl.class.getName());

	private	BlendingMonitorBean		myConfigBean = null;
	private	boolean					myNativeLayerInitialized = false;

	// BlendingFacade and ServoControlFacade are used only if we DON'T have a roboard.
	private InternalBlendingFacade  myBlendingFacade;
	// This is not (yet) used after init.
	private ServoControlFacade		myServoControlFacade;

	private	BlendingAnimator		myBlendingAnimator;
	
	// private ServoChannelConfig		myServoChannelConfigs[];
	
	private String					myAnimDirPath;

	private IServoPositionReporter	myServoPositionReporter;
	
	
	public BlendingMonitorImpl() {
	}
	
	public void setConfigBean(BlendingMonitorBean bmb) {
		theLogger.fine("BMI.setConfigBean()");
		myConfigBean = bmb;
		myConfigBean.addPropertyChangeListener(this);
	}
	private synchronized boolean ensureInitialized() {
		if (!myNativeLayerInitialized) {
			// Need to set this flag immediately, because some of this activity may trigger
			// property events that call this method recursively!
			// (Specifically, the gaze animator init).
			myNativeLayerInitialized = true;
			ConfigSystemImpl csi = (ConfigSystemImpl) lookupSubsystem(ConfigSystemImpl.class);
			try {
				theLogger.info("************** BlendingMonitorImpl is initializing **************");
				Parameters roboParams = csi.getTargetRobotParameters();
				Parameters csParams = roboParams.getParam("ControlSystem").getChildren();
				Parameters hcParams = csParams.getParam("HeadControlSystem").getChildren();
				Parameters blenderParams = csParams.getParam("SimpleBlender").getChildren();
				String roboardURL = hcParams.getChildValue("RoboardURL");
				if ((roboardURL != null) && (roboardURL.toUpperCase().equals("NULL"))) {
					roboardURL = null;
				}
				if (roboardURL == null) {
					theLogger.info("**************  No roboard present, initing servo control facade **************");
					myServoControlFacade =  new ServoControlFacade(hcParams);
					myServoControlFacade.startup();
					theLogger.info("**************  init native blending facade ************** ");
					myBlendingFacade = new InternalBlendingFacade(blenderParams);
	
					myBlendingFacade.startup();
					theLogger.info("**************  native blending facade init complete    ***********");
				}
				myAnimDirPath = blenderParams.getChildValue("AnimationDir");
				String servoConfigFilename = hcParams.getChildValue("ServoConfig");
						
				initBlendingAnimator(blenderParams, hcParams, servoConfigFilename);
				AnimoidFacade af = myBlendingAnimator.getAnimoidFacade();
				myServoPositionReporter = myBlendingAnimator;
				

				theLogger.info("**************  BMI init complete **************");
				
			} catch (Throwable t) {
				theLogger.log(Level.SEVERE, "Error initializing Blending system", t);
				myNativeLayerInitialized = false;
			}
			// This will trigger init of VisionMonitorChannelImpl if needed
			// Note that underlying videoduct is not necessarily running (or not running).
			// VisionFacade vf = com.hansonrobotics.gui.monitor.Main.theVMCI.retrieveVisionFacade();
		}
		return myNativeLayerInitialized;
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
	public BlendingAnimator getBlendingAnimator() {
		ensureInitialized();
		return myBlendingAnimator;
	}
	public IServoPositionReporter getServoPositionReporter() {
		return myServoPositionReporter;
	}
	public String getAnimDirPath() {
		ensureInitialized();
		return myAnimDirPath;
	}
	private void initBlendingAnimator(Parameters blenderParams, 
				Parameters headControlParams, String servoConfigFilename) throws Throwable {
		String animoidConfigPath = blenderParams.getChildValue("AnimoidConfigFile");
		String visemeConfigPath = blenderParams.getChildValue("NewVisemeConfigFile");
		String msecPerFrameString = blenderParams.getChildValue("MillisecPerFrame");
		String frameDurationSmoothingFactorString = headControlParams.getChildValue("FrameDurationSmoothingFactor");
		Integer msecPerFrame = Integer.parseInt(msecPerFrameString);
		Double frameDurationSmoothingFactor = Double.parseDouble(frameDurationSmoothingFactorString);
		// We need to talk to the Vision system in order to do gaze control.
		// TODO:  Disable this linkage in cases where we have no camera or don't want gaze control.
		VisionMonitorChannelImpl vmci = (VisionMonitorChannelImpl) lookupSubsystem(VisionMonitorChannelImpl.class); 		
		myBlendingAnimator = new BlendingAnimator(myBlendingFacade, servoConfigFilename, animoidConfigPath, 
					visemeConfigPath, msecPerFrame, frameDurationSmoothingFactor, vmci, myConfigBean);

	}
	public MotionController getMotionController() {
		ensureInitialized();
		MotionControlProxy mcp = new MotionControlProxy(myConfigBean);
		return mcp;
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
		if (propertyName.equals(BlendingMonitorBean.PROP_USING_ANY_FACE_ATTENTION_RULE)) {
            if (propertyValue.equals(Boolean.TRUE)) {
                theLogger.info("usingAnyFaceAttentionRule property is now TRUE!");
				enableAttentionGaze();
            } else {
                theLogger.info("usingAnyFaceAttentionRule property is now FALSE!");
				disableAttentionGaze();
            }
		} else if (propertyName.equals(BlendingMonitorBean.PROP_USING_JOYSTICK_GAZE_CONTROL)) {
            if (propertyValue.equals(Boolean.TRUE)) {
                theLogger.info("usingJoystickGazeControl property is now TRUE!");
				myBlendingFacade.enableJoystickGazeControl();
            } else {
                theLogger.info("usingJoystickGazeControl property is now FALSE!");
				myBlendingFacade.disableJoystickGazeControl();
            }
		} else if (propertyName.equals(BlendingMonitorBean.PROP_USING_VISEME_RULE)) {
			if (af != null) {
				if (propertyValue.equals(Boolean.TRUE)) {
					theLogger.info("usingVisemeRule property is now TRUE!");
					af.enableVisemes();
				} else {
					theLogger.info("usingVisemeRule property is now FALSE!");
					af.disableVisemes();
				}
			}
		} else if (propertyName.equals(BlendingMonitorBean.PROP_USING_BLINK_RULE)) {
            if (propertyValue.equals(Boolean.TRUE)) {
                theLogger.info("usingBlinkRule property is now TRUE!");
				myBlendingFacade.activateBlinkRule();
            } else {
                theLogger.info("usingBlinkRule property is now FALSE!");
				myBlendingFacade.deActivateBlinkRule();
            }
		} else if (propertyName.equals(BlendingMonitorBean.PROP_USING_BROW_BLEND_RULE)) {
            if (propertyValue.equals(Boolean.TRUE)) {
                theLogger.info("usingBrowBlendRule property is now TRUE!");
				myBlendingFacade.activateBrowBlendRule();
            } else {
                theLogger.info("usingBrowBlendRule property is now FALSE!");
				myBlendingFacade.deActivateBrowBlendRule();
			}
		} else if (propertyName.equals(BlendingMonitorBean.PROP_USING_DEFAULT_RULE)) {
            if (propertyValue.equals(Boolean.TRUE)) {
                theLogger.info("usingDefaultRule property is now TRUE!");
				myBlendingFacade.activateDefaultRule();
            } else {
                theLogger.info("usingDefaultRule property is now FALSE!");
				myBlendingFacade.deActivateDefaultRule();
            }
		} else if (propertyName.equals(BlendingMonitorBean.PROP_USING_SCRIPTED_ANIMS)) {
			if (af != null) {
				if (propertyValue.equals(Boolean.TRUE)) {
					theLogger.info("usingScriptedAnims property is now TRUE!");
					af.enableScriptedAnimations();
				} else {
					theLogger.info("usingScriptedAnims property is now FALSE!");
					af.disableScriptedAnimations();
				}
			}
		}
	}
	public void enableAttentionGaze() {
		ensureInitialized();
		AnimoidFacade af = getAnimoidFacade();
		if (af != null) {
			af.enableAttentionGaze();
		}
	}
	public void disableAttentionGaze() {
		ensureInitialized();
		AnimoidFacade af = getAnimoidFacade();
		if (af != null) {
			af.disableAttentionGaze();
		}
	}
	public void resetServos() {
		ensureInitialized();
		AnimoidFacade	af = getAnimoidFacade();
		if (af != null) {
			af.forceServosToCenter();
		}
	}
	public void stopAllAnimations() {
		ensureInitialized();
		AnimoidFacade	af = getAnimoidFacade();		
		if (af != null) {
			af.killAllAnimations();
		}
	}

}