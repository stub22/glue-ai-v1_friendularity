/*
 * VisionFacade.java
 *
 * Created on Jul 17, 2007, 10:33:36 PM
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package org.friendularity.sight.vision;

import org.cogchar.zzz.nwrap.core.NativeEngine;
import java.awt.Graphics;

/**
 *
 * @author josh
 */
public class VisionFacade extends NativeEngine {
	// This value is a C++ pointer of type:  VisionFacadeWrapper *
    private long m_vp;
    
    public long raw() {
        return m_vp;
    }
    
    public VisionFacade() {
        startup();
    }
    
    public native void CreateFacade();
    public native void configure(String config);
    public native void Activate();
    public native void DeActivate();
    public native void setInverted(boolean val);
	public native void setMotionDetectParams(int historyDur, int segDurThresh, int deltaAmpThresh, int resultAmpThresh);


	// Stu sez:  It looks like these actually "add" observers, so multiple of each 
	// type can coexist.
    public native void SetRawVisionObserver(IRawFrameObserver ob);
    public native void SetFaceDetectObserver(IROIObserver ob);
    public native void SetMotionDetectObserver(IROIObserver ob);
    public native void SetFaceTrackObserver(ITrackObserver ob);
    
    public native void UnSetRawVisionObserver(IRawFrameObserver ob);
    public native void UnSetFaceDetectObserver(IROIObserver ob);
    public native void UnSetMotionDetectObserver(IROIObserver ob);
    public native void UnSetFaceTrackObserver(ITrackObserver ob);


    public void startup() {
		// Creates a new VisionFacadeWrapper in the C++ layer, and
		// sets our m_vp pointer to its address.
        CreateFacade();
		
    }
    
    public void shutdown() {
        throw new UnsupportedOperationException("Not supported yet.");
    }
    
}
