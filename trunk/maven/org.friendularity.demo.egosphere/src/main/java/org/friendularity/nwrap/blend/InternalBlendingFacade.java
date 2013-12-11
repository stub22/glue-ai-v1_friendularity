/**
 * Copyright 2008 Hanson Robotics Inc.
 * All Rights Reserved.
 */

package org.friendularity.nwrap.blend;

import org.cogchar.zzz.ancient.utility.Parameters;
import org.cogchar.zzz.nwrap.core.NativeEngine;

/**
 * @author Stu Baurmann, Josh Varner
 */
public class InternalBlendingFacade extends NativeEngine {

	private Parameters		myParams;
	
	public InternalBlendingFacade(Parameters p) {
		myParams = p;
	}
	public void startup() {
	     startup(myParams.toString());
	}

	private native void startup(String params);
    public native void shutdown();
    
	private native void sendBlenderCommandNative(String command, String arg);
	
    private native void playEmotionNative(String emotion);
    private native void deActivateGazeControlNative();
    private native void activateGazeControlNative();
    private native void deActivateVisemeNative();
    private native void activateVisemeNative();
	private native void startWatchingFacesNative();
	private native void stopWatchingFacesNative();

    public void disableJoystickGazeControl() { deActivateGazeControlNative(); }
    public void enableJoystickGazeControl() { activateGazeControlNative(); }
    public void disableOldNativeVisemes() { deActivateVisemeNative(); }
    public void enableOldNativeVisemes() { activateVisemeNative(); }

	/*
	public void playAnimation(String animName) {
		sendBlenderCommand("PLAY_ANIMATION", animName);
	}
	 */
	public void sendBlenderCommand (String command, String arg) {
		if (command == null) {
			throw new RuntimeException("sendBlenderCommand invoked with null command");
		}
		if (arg == null) {
			arg = "NULL";
		}
		sendBlenderCommandNative(command, arg);
	}
	/*
    public void playEmotion(String emotion) {
        playEmotionNative(emotion);
    }

	public void startWatchingFaces() {
		startWatchingFacesNative();
	}
	public void stopWatchingFaces() {
		stopWatchingFacesNative();
	}
	*/
	public void activateBlinkRule() {
		sendBlenderCommand("ACTIVATE_RULE", "BLINK");
	}
	public void deActivateBlinkRule() {
		sendBlenderCommand("DEACTIVATE_RULE", "BLINK");
	}
	public void activateBrowBlendRule() {
		sendBlenderCommand("ACTIVATE_RULE", "BROW_BLEND");
	}
	public void deActivateBrowBlendRule() {
		sendBlenderCommand("DEACTIVATE_RULE", "BROW_BLEND");
	}
	public void activateDefaultRule() {
		sendBlenderCommand("ACTIVATE_RULE", "DEFAULT");
	}
	public void deActivateDefaultRule() {
		sendBlenderCommand("DEACTIVATE_RULE", "DEFAULT");
	}
	public void activateTransformRule() {
		sendBlenderCommand("ACTIVATE_RULE", "TRANSFORM");
	}
	public void deActivateTransformRule() {
		sendBlenderCommand("DEACTIVATE_RULE", "TRANSFORM");
	}	
}
