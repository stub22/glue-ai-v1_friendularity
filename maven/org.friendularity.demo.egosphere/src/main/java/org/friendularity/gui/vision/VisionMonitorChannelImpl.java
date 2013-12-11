/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.friendularity.gui.vision;

import java.util.Observer;
import java.util.Observable;

import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeEvent;

import java.awt.Color;


import org.friendularity.app.face.FaceDetectNoticeProcessor;
import org.friendularity.app.face.FaceHypothesis;
import org.friendularity.app.face.FaceModel;
import org.friendularity.app.freckle.FreckleFace;
import org.friendularity.app.freckle.FreckleMatcher;
import org.friendularity.app.jmxwrap.SignalStation;

import java.util.ArrayList;
import java.util.logging.Logger;
import org.cogchar.zzz.ancient.utility.NullPaddedArrayList;
import org.cogchar.zzz.ancient.utility.Parameters;
import org.cogchar.zzz.oldboot.ConfigSystemImpl;
import org.cogchar.zzz.oldboot.SubsystemImpl;
import org.cogchar.sight.api.obs.FaceTrackObserver;

import org.cogchar.sight.api.obs.ROIVisionObserver;
import org.cogchar.sight.api.obs.RawFrameProcessor;
import org.cogchar.sight.api.obs.VisionFacade;
import org.freckler.sight.impl.motion.MotionFilterObserver;
import org.jdesktop.observablecollections.ObservableList;


/**
 * @author Stu Baurmann (based in part on code by Bob Hauser and Josh Varner)
 */
public class VisionMonitorChannelImpl extends SubsystemImpl implements Observer, PropertyChangeListener {
    private static Logger	theLogger = Logger.getLogger(VisionMonitorChannelImpl.class.getName());
    private VisionFacade                        myVisionFacade;
    private RawFrameProcessor                   myRawFrameProcessor;
    
    private ROIVisionObserver                   myFaceDetectObs;
	private	MotionFilterObserver				myMotionDetectObs;
    private FaceTrackObserver                   myFaceTrackObs;
    
    private VisionSwingPanel                    myDrawingPanel;
    private VisionMonitorChannelBean            myChannelConfigBean;
    
	private	boolean								myNativeLayerInitialized = false;
	
	private	FaceModel							myFaceModel;
	private	FreckleMatcher						myFreckleMatcher;
	// private ObservableList<FaceHypothesis>		myObservableHypothesisList;
	
    public VisionMonitorChannelImpl() {
		// RawFrameProcessor is pure java, and existence of this processor makes it easier to 
		// hook up the VisionSwingPanel.
		myRawFrameProcessor = new RawFrameProcessor();
		myRawFrameProcessor.addObserver(this);
		
		SignalStation.theVMCI = this;
	
		// Using NullPaddedArrayList helps, but does not completely  prevent exceptions in the swing table
		// binding, because it creates its own ArrayList of "wrappers", and it is
		// accessing THOSE that causes the exceptions.  
		//   https://beansbinding.dev.java.net/issues/show_bug.cgi?id=9
		ArrayList<FaceHypothesis> innerHypoList = new NullPaddedArrayList<FaceHypothesis>();
		//  new ArrayList<FaceHypothesis>();
		// myObservableHypothesisList = ObservableCollections.observableList(innerHypoList);
		myFaceModel = new FaceModel();
		myFaceModel.setRawFrameProcessor(myRawFrameProcessor);
		myFreckleMatcher = new FreckleMatcher();
		myFreckleMatcher.setBatchSupplier(myFaceModel);
		myFaceModel.setFreckleMatcher(myFreckleMatcher);
		
		// Defer everything else to be created by clients or in response to events.
    }
	/* All bean+GUI stuff needs to be able to happen without this having
	 * happened.
	 */
	private void ensureInitialized() {
		if (!myNativeLayerInitialized) {
			myVisionFacade = new VisionFacade();

			// This adds another vision processor at the END of the processor chain.
			// But if we call this before "configure()", it should be
			// at the beginning of the chain, right?
			myVisionFacade.SetRawVisionObserver(myRawFrameProcessor);

			// This causes the chain of low level C++ Vision Processors to be filled
			// in with face detect, face track, motion detect, etc.
			configureFacadeAndPersonDB();
	
			myNativeLayerInitialized = true;
		} 
	}
	/*  Calling SetRawVisionObserver TWICE for the same processor argument
	 *  is not good.  The meaning is actually "AddRawVisionObserver", so this
	 *  call can lead to duplication of observers, which means duplication of
	 * video frame delivery.
	public void resetObserver(){
		myVisionFacade.SetRawVisionObserver(myRawFrameProcessor);
	}
	 */

	public ObservableList<FreckleFace> getObservableKnownFaceList() {
		return myFreckleMatcher.getInventory().getObservableKnownFaceList();
	}

	public VisionFacade retrieveVisionFacade() {
		ensureInitialized();
		return myVisionFacade;
	}
	
	private Parameters getVisionParameters() {
		Parameters vParams = null;
		ConfigSystemImpl csi = (ConfigSystemImpl) lookupSubsystem(ConfigSystemImpl.class);
		Parameters roboParams = csi.getTargetRobotParameters();
		
		if (roboParams.hasParam("Vision")) {		
			vParams = roboParams.getParam("Vision").getChildren();
		}
		return vParams;
	}
	private void configureFacadeAndPersonDB() {
/*		String ParametersString = 
			"HaarFaceDetectCascadeFile=C:\\hri\\Robots\\_common\\open_cv\\haarcascade_frontalface_alt.xml\n"
			+ "FaceRecServerIP=127.0.0.1\n"
			+ "FaceRecServerPort=57001\n"
			+ "FaceRecPopulation=c:\\Documents and Settings\\josh\\Desktop\\camPop\\compiledPopulation.fvp";
*/
		Parameters vParams = getVisionParameters();
		String	visionParamString = null;
		if (vParams != null) {		
			visionParamString = vParams.toString();
			myFaceModel.configure(vParams);
			myFreckleMatcher.configureServerOrConnectProxy(vParams);
		}
		theLogger.fine("VisionParams:\n" + visionParamString);
		if (visionParamString == null) {
			throw new RuntimeException("Can't resolve SpeechRecognition params.");
		}		
		
		myVisionFacade.configure(visionParamString);
        // myPersonDatabase.Configure(visionParamString, myVisionFacade);
    }   
    public void setDrawingPanel(VisionSwingPanel drawingPanel) {
        myDrawingPanel = drawingPanel;
        myDrawingPanel.setImageSource(myRawFrameProcessor);   
    }
    public void setConfigBean(VisionMonitorChannelBean configBean) {
		theLogger.fine("VMCI: setConfigBean()");
        myChannelConfigBean = configBean;
        myChannelConfigBean.addPropertyChangeListener(this); 
    }
    public void propertyChange(PropertyChangeEvent evt) {
		String propertyName = evt.getPropertyName();
		Object propertyValue = evt.getNewValue();
		theLogger.fine("ChannelImpl got property change:  " + propertyName + " := " + propertyValue);
		if (java.beans.Beans.isDesignTime()) {
			theLogger.fine("It's design time!  No further processing of event");
			return;
		}
		ensureInitialized();
		if (propertyName.equals(VisionMonitorChannelBean.PROP_GRABBING_FRAMES)) {
            if (propertyValue.equals(Boolean.TRUE)) {
                theLogger.fine("grabbingFrames property is now TRUE!");
				startForwardingFrames();
            } else {
                theLogger.fine("grabbingFrames property is now FALSE!");
				stopForwardingFrames();
            }
        } else if (propertyName.equals(VisionMonitorChannelBean.PROP_DETECTING_FACES)) {
			if (propertyValue.equals(Boolean.TRUE)) {
				theLogger.fine("Starting face-detect animation");
				myFaceDetectObs = new FaceDetectNoticeProcessor(Color.red);
				myVisionFacade.SetFaceDetectObserver(myFaceDetectObs);
				myRawFrameProcessor.AddAnnotater(myFaceDetectObs);
				((FaceDetectNoticeProcessor) myFaceDetectObs).setFaceModel(myFaceModel);
				myRawFrameProcessor.AddAnnotater(myFaceModel.getAnnotater());
			} else {
				theLogger.fine("Stopping face-detect animation");
				myVisionFacade.UnSetFaceDetectObserver(myFaceDetectObs);
				myRawFrameProcessor.RemoveAnnotater(myFaceDetectObs);
				myRawFrameProcessor.RemoveAnnotater(myFaceModel.getAnnotater());
				myFaceDetectObs = null;
			}
		} else if (propertyName.equals(VisionMonitorChannelBean.PROP_DETECTING_MOTION)) {
			if (propertyValue.equals(Boolean.TRUE)) {
				theLogger.fine("Starting motion-detect animation");
				myMotionDetectObs = new MotionFilterObserver(Color.green, Color.orange);
				myVisionFacade.SetMotionDetectObserver(myMotionDetectObs);
				myRawFrameProcessor.AddAnnotater(myMotionDetectObs);
			} else {
				theLogger.fine("Stopping motion-detect animation");
				myVisionFacade.UnSetMotionDetectObserver(myMotionDetectObs);
				myRawFrameProcessor.RemoveAnnotater(myMotionDetectObs);
				myMotionDetectObs = null;
			}
		} else if (propertyName.equals(VisionMonitorChannelBean.PROP_TRACKING_FACES)) {
			if (propertyValue.equals(Boolean.TRUE)) {
				theLogger.fine("Starting face-tracking monitoring");
				myFaceTrackObs = new FaceTrackObserver();
				myVisionFacade.SetFaceTrackObserver(myFaceTrackObs);
				myRawFrameProcessor.AddAnnotater(myFaceTrackObs);
				// myVisionFacade.SetFaceTrackObserver(myFaceModel);
				
			} else {
				theLogger.fine("Stopping face-tracking monitoring");
				myVisionFacade.UnSetFaceTrackObserver(myFaceTrackObs);
				myRawFrameProcessor.RemoveAnnotater(myFaceTrackObs);
				myFaceTrackObs = null;
			}
		} else if (propertyName.equals(VisionMonitorChannelBean.PROP_RECOGNIZING_FACES)) {
			if (propertyValue.equals(Boolean.TRUE)) {
				theLogger.info("Starting freckle matching");
				myFreckleMatcher.startMatching();
				
			} else {
				theLogger.info("Stopping freckle matching");
				myFreckleMatcher.stopMatching();
			}
		} else if (propertyName.equals(VisionMonitorChannelBean.PROP_FLIPPING_VIDEO_VERTICAL)) {
			myVisionFacade.setInverted(propertyValue.equals(Boolean.TRUE));
		}
    }
    public void startForwardingFrames() {
		ensureInitialized();
		myVisionFacade.setInverted(myChannelConfigBean.isFlippingVideoVertical());
		// First time this is called, it leads to (in C++)
		// VisionProcessor->ProcessDuct(0)   - which opens the Videoduct
		// Subsequent times it just reenables our callback
        myVisionFacade.Activate();
    }
	public void stopForwardingFrames() {
		ensureInitialized();
		// Disables our callback, but leaves videoduct open
		myVisionFacade.DeActivate();
    }

    public void update (Observable o, Object arg) {
		// theLogger.fine("VMCI: update called");
        myDrawingPanel.repaint();
    }
	public ROIVisionObserver getFaceDetectObserver() {
		return myFaceDetectObs;
	}
	public FaceTrackObserver getFaceTrackObserver() {
		return myFaceTrackObs;
	}
	public MotionFilterObserver getMotionDetectObserver() {
		return myMotionDetectObs;
	}
	public RawFrameProcessor getRawFrameProcessor() {
		return myRawFrameProcessor;
	}
	public FaceModel getFaceModel() {
		return myFaceModel;
	}
	public void setMotionDetectParams(int historyDur, int segDurThresh, int deltaAmpThresh, int resultAmpThresh) {
		myVisionFacade.setMotionDetectParams(historyDur, segDurThresh, deltaAmpThresh, resultAmpThresh);
	}
        /*HEREHERE - figure out clean shutdown
     public void Finalize() {
        m_vision.DeActivate();
    }*/
        
}
