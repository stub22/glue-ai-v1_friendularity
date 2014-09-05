/*
 *  Copyright 2008 Hanson Robotics Inc.
 *  All Rights Reserved.
 */

package org.friendularity.app.animation;

import org.friendularity.app.face.FaceModel;
import org.friendularity.nwrap.blend.InternalBlendingFacade;
import org.friendularity.gui.blending.BlendingMonitorBean;
import org.friendularity.gui.vision.VisionMonitorChannelImpl;
import org.friendularity.nwrap.joint.JointFrameStruct;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.List;
import java.util.logging.Logger;
import javax.swing.SwingUtilities;
import org.cogchar.animoid.broker.AnimoidFacade;
import org.cogchar.animoid.calc.estimate.PositionEstimator;

import org.cogchar.animoid.monitor.IServoMonitor;
import org.cogchar.animoid.monitor.IServoPositionReporter;

import org.cogchar.api.animoid.config.bonus.AnimoidConfig;
import org.cogchar.api.animoid.protocol.Frame;
import org.cogchar.api.animoid.protocol.JPARFrame;
import org.cogchar.api.animoid.protocol.JointPositionSnapshot;
import org.cogchar.api.animoid.protocol.JointStateCoordinateType;

import org.friendularity.gaze.api.AnimoidGazeFacade;
import org.friendularity.gaze.api.AnimoidGazeConfig;
/**
 * @author Stu Baurmann
 */
public class BlendingAnimator implements PropertyChangeListener, IServoPositionReporter {
	private static Logger	theLogger = Logger.getLogger(BlendingAnimator.class.getName());

	private BlendingMonitorBean				myBlendingMonitorBean;
	private	AnimoidGazeFacade				myAnimoidFacade;

	// Single listener for servo pos updates - redo this as a listener reg pattern.
	private	IServoMonitor					myServoMonitor;

	// Hacked up vision link to support gaze control.
	private	VisionMonitorChannelImpl		myVMCItemp;

	// If we are running a native blender, then we will have a jointFrameTransformer
	// which reports lopsided frames.
	private	JointFrameTransformer			myJointFrameTransformer;
	private	JointPositionSnapshot			myLastInputFrameSnapshotLopsided;

	public BlendingAnimator(InternalBlendingFacade ibf, String servoConfigPath, String animoidConfigPath, 
				String visemeConfigPath, Integer msecPerFrame, Double frameDurationSmoothingFactor,
				VisionMonitorChannelImpl vmci, BlendingMonitorBean bmb)
				throws Throwable {
		theLogger.fine("*************************** BA constructor start");
		myBlendingMonitorBean = bmb;
		// Used below to setup gaze control.
		myVMCItemp = vmci;
		
		theLogger.info("BlendingAnimator - reading servo config file from: " + servoConfigPath);
		theLogger.info("BlendingAnimator - setting up AnimoidFacade");
		myAnimoidFacade = new AnimoidGazeFacade(servoConfigPath, null, // animoidConfigPath, 
				visemeConfigPath, msecPerFrame, frameDurationSmoothingFactor);
		
		AnimoidGazeConfig animConf = (AnimoidGazeConfig) myAnimoidFacade.getAnimoidConfig();
		
		if (animConf != null) {
			List gazePlanNameList = animConf.getGazeStrategyNameList();
			myBlendingMonitorBean.setAvailableGazeStrategyList(gazePlanNameList);
			myBlendingMonitorBean.addPropertyChangeListener(this);
		}		
		if (ibf != null) {
			initJointFrameTransformer(ibf);
		} else {
			theLogger.info("No native blendingFacade present, skipping jointTransformer setup [presumably we are using roboard!]");
		}


		theLogger.info("*************************** BA - constructor end");
    }
	private void initJointFrameTransformer(InternalBlendingFacade ibf) throws Throwable {
		theLogger.info("BA - initializing JointFrame transformer");
		myJointFrameTransformer = JointFrameTransformer.setup(ibf);
		myJointFrameTransformer.setDelegate(this);
		ibf.activateTransformRule();
	}
	public void completeInitWhenJobBrokerAvailable() {
		myAnimoidFacade.setupBlenderJob();
		if (myVMCItemp != null) {
			FaceModel fm = myVMCItemp.getFaceModel();
			myAnimoidFacade.setSightModel(fm);
			// Establish linkage used for gaze control.
			fm.setAnimator(myAnimoidFacade);
			myAnimoidFacade.setTestMotionJobs();
			/*
			 *
		myGazeCalcAnnotater = new GazeCalculationAnnotater();
		RawFrameProcessor rfp = vmci.getRawFrameProcessor();
		rfp.AddAnnotater(myGazeCalcAnnotater);
			 */
		}		
	}
	public FaceModel getFaceModel() {
		return myVMCItemp.getFaceModel();
	}
	public synchronized void setMonitor(IServoMonitor monitor) {
		myServoMonitor = monitor;
	}

	@Override synchronized public JPARFrame getServoSnapshotAROM() {
		JPARFrame jparSnapshotFrame = null;
		if (myLastInputFrameSnapshotLopsided != null) {
			Frame previousFrameAbsRom = myLastInputFrameSnapshotLopsided.copyAndConvert(JointStateCoordinateType.FLOAT_ABS_RANGE_OF_MOTION);
			jparSnapshotFrame = JPARFrame.makeFrom(previousFrameAbsRom);
		}
		return jparSnapshotFrame;
	}

	@Override synchronized public JointPositionSnapshot getServoSnapshotLopsided() {
		// This is a snapshot of last "servo snapshot" from C++ side of TransformRule.
		// It does not reflect what is usually the last command sent from java side,
		// or how it was modified by C++ side (e.g. adding Joystick input).
		// So, it is an OLD (by about one frame) estimate of "where we are now".
		return myLastInputFrameSnapshotLopsided;
	}

	public AnimoidGazeFacade getAnimoidFacade() {
		return (AnimoidGazeFacade) myAnimoidFacade;
	}
	public PositionEstimator getPositionEstimator() {
		return myAnimoidFacade.getPositionEstimator();
	}
	public synchronized void doAnimationTransform(JointFrameStruct inJFS, JointFrameStruct outJFS) {

		// Note that we are blocking the C++ animation thread in this execution,
		// so we mustn't tarry!
		// double secondsPerFrame = myAnimoidFacade.getSecondsPerFrame();


		final JointPositionSnapshot lopsidedSnap = myJointFrameTransformer.makeLopsidedFrameSnapshotFromJFS(inJFS);
		myLastInputFrameSnapshotLopsided = lopsidedSnap;
		final JPARFrame previousFrameAbsRom = getServoSnapshotAROM();
		Frame nextFrameAbsRom = myAnimoidFacade.transformFrame(previousFrameAbsRom);
		outJFS.overwriteWithAbsRomData(nextFrameAbsRom);
		if (myServoMonitor != null) {
			SwingUtilities.invokeLater(new Runnable() {
				public void run() {
					// Posts a GUI update to servos tab
					myServoMonitor.servoSnapshotUpdate(lopsidedSnap, previousFrameAbsRom);
				}});
		}
	}
	public void propertyChange(PropertyChangeEvent evt) {
		String propertyName = evt.getPropertyName();
		Object propertyValue = evt.getNewValue();
		// theLogger.fine("BlendingAnimator got property change:  " + propertyName + " := " + propertyValue);
		if (java.beans.Beans.isDesignTime()) {
			theLogger.fine("It's design time!  No further processing of event");
			return;
		}
		if (propertyName.equals(BlendingMonitorBean.PROP_GAZE_STRATEGY_NAME)) {
			String gazePlanName = (String) propertyValue;
			theLogger.fine("propGazePlan is now " +  gazePlanName);
			myAnimoidFacade.suggestGazeStrategyName(gazePlanName);
		}
		if (propertyName.equals(BlendingMonitorBean.PROP_GAZE_FACE_NUMBER)) {
			Integer gazeFaceNumber = (Integer) propertyValue;
			theLogger.fine("propGazeFaceNumber is now " +  gazeFaceNumber);
			// TODO: Lookup the right PersonTracker and set that as target.
			// myAnimoidFacade.suggestGazeSightNumber(gazeFaceNumber);
		}		
	}

}
