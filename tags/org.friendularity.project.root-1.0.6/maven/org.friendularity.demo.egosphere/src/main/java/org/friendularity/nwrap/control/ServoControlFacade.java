/*
 * ServoControlFacade.java
 *
 * Created on Aug 10, 2007, 2:14:02 PM
 *
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.friendularity.nwrap.control;

import org.cogchar.zzz.ancient.utility.Parameters;
import org.cogchar.zzz.nwrap.core.NativeEngine;

/**
 *
 * @author humankind
 */
public class ServoControlFacade extends NativeEngine {
	
	private Parameters myParams;
    public ServoControlFacade(Parameters p) {
        myParams = p;
    }
	
	public void startup() {
		String paramString = myParams.toString();
		System.out.println("ServoControlFacade is starting up with paramString: " + paramString);
		startup(paramString);
	}
	// Calling this fires up the native C++ version of SystemManager, in all its glory.
    private native void startup(String params);

    public native void shutdown();
    
    public native int getGyroRx();
    public native int getAccelVy();
    public native int getAccelVz();
    public native int getBatteryLevel();
    public native int[] getRawFrame();
    

}
