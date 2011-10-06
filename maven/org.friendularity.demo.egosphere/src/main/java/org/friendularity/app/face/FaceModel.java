/*
 *  Copyright 2008 Hanson Robotics Inc.
 *  All Rights Reserved.
 */

package org.friendularity.app.face;

import org.friendularity.app.freckle.FreckleMatchBatch;
import org.friendularity.app.freckle.FreckleMatcher;
import org.friendularity.app.freckle.FreckleMatchCandidate;

import org.friendularity.app.jmxwrap.SignalStation;
import org.friendularity.gui.vision.FaceModelAnnotater;

import java.awt.Rectangle;
import java.util.ArrayList;
import java.util.List;
import java.util.Observable;
import java.util.Observer;
import java.util.logging.Logger;
import org.cogchar.ancient.utility.Parameters;
import org.cogchar.animoid.calc.estimate.GazeDirectionComputer;
import org.cogchar.animoid.calc.estimate.TargetObjectStateEstimate;
import org.cogchar.animoid.config.FaceNoticeConfig;
import org.cogchar.animoid.protocol.EgocentricDirection;
import org.cogchar.animoid.protocol.Frame;
import org.cogchar.integroid.boot.ThreadAwareObject;
import org.cogchar.platform.util.CollectionUtils;
import org.cogchar.platform.util.TimeUtils;
import org.cogchar.sight.hypo.SightHypothesis;
import org.cogchar.sight.hypo.SightHypothesis.ActivationStatus;
import org.cogchar.sight.hypo.SightModel;
import org.cogchar.vision.IAnnotatingObserver;
import org.cogchar.vision.PortableImage;
import org.cogchar.vision.RawFrameProcessor;

/**
 * The ITrackObserver native-connected infrastructure is not being used right now.
 * Instead, FaceModel is serving as the hub of our FaceObservation/FaceHypothesis system.
 * @author Stu Baurmann
 */
public class FaceModel extends SightModel<FaceHypothesis> implements FreckleMatchBatch.Supplier {

	private static Logger theLogger = Logger.getLogger(FaceModel.class.getName());

	private	ThreadAwareObject				myTAO;
	// private ObservableList<FaceHypothesis>	myObservableHypotheses;	
	private RawFrameProcessor				myRawFrameProcessor;
	private RawFrameObserver				myRFO;
	private FaceModelAnnotater				myAnnotater;
	private Parameters						myVisionParameters;
	
	private FreckleMatcher					myFreckleMatcher;

	private static	double	IMAGE_EXPANSION_MARGIN_RATIO = 0.25;
	private static	long	HYPO_FRECKLE_MATCH_SLACK_MSEC = 800L;
	private static  long	FRECKLE_BATCH_SLEEP_TIME_MSEC = 100L;
	
	public FaceModel() {
		super();
		myTAO = new ThreadAwareObject();
	}

	public void configure(Parameters visionParameters) {
		myVisionParameters = visionParameters;
	}

	public IAnnotatingObserver getAnnotater() {
		if (myAnnotater == null) {
			myAnnotater = new FaceModelAnnotater(this);
		}
		return myAnnotater;
	}
/*
 For some weird reason this one gets called by face tracker.  JNI confusion about different
 *  dimensions of int arrays?
	public void ProcessFrame(int[] rectData) {
		// IROIObserver method - called with face-detection data
		throw new UnsupportedOperationException("Not supported yet.");
	}
	private void processTrackedFace(int[] trackedFaceData) {
		myTAO.blessCurrentThread();
		
		int[] tfd = trackedFaceData;
		List<Point>		faceCenterRecentHistory = new ArrayList<Point>();
		// Size of the point array is hardcoded in C++ :  currently 5
		// Last point in the array is the most recent
		for (int j = 0; j < tfd.length; j += 2) {
			int x = tfd[j];
			int y = tfd[j+1];
			Point p = new Point(x, y);
			faceCenterRecentHistory.add(p);
		}
		// theLogger.finer("processTrackedFace - built pointHistory: " + faceCenterRecentHistory);
	}

	public void ProcessFrame(int[][] trackData) {
		// ITrackObserver method - called with face-tracking data
		myTAO.blessCurrentThread();
		for (int i = 0; i < trackData.length; i++) {
			processTrackedFace(trackData[i]);
		}
	}
*/
	public void setRawFrameProcessor(RawFrameProcessor rfp) {
		if ((myRawFrameProcessor == null) && (myRFO == null)) {
			myRawFrameProcessor = rfp;
			myRFO = new RawFrameObserver(rfp);
		} else {
			throw new RuntimeException("setRawFrameProcessor called, but myRawFrameProc=" + myRawFrameProcessor
						+ " and myRFO=" + myRFO);
		}
	}

	public EgocentricDirection getCameraCenterDirection(boolean enhancedAccuracy) {
		EgocentricDirection egoDir = null;
		Frame jointPosSnap = getJointPositionEstimateForCurrentVideoFrame(); // getJointPosSnapNow(enhancedAccuracy);
		if (jointPosSnap != null) {
			egoDir = getGazeDirectionComputer().computeGazeCenterDirection(jointPosSnap);
		}
		return egoDir;
	}

	/* This method is called in the face-detection callback, which means it
	 * is on the native vision thread, and so should return as quickly as possible.
	 */
	public synchronized void facesSeen(List<Rectangle> faceRects) {
		// We use the timestamp set by videoFrameTick().
		long videoTimestamp = getTimestampForCurrentVideoFrame();
		long nowStamp = TimeUtils.currentTimeMillis();
		double videoStampAge = TimeUtils.getStampAgeSec(videoTimestamp);
		// theLogger.info("facesSeen at " + nowStamp + ", which is " + videoStampAge + " sec after video" + videoTimestamp);
		Frame cameraCenterPosEstimate = getJointPositionEstimateForCurrentVideoFrame(); // getJointPosSnapNow(true);
		if (cameraCenterPosEstimate == null) {
			theLogger.fine("Faces seen but cannot get camera center pos estimate!");
			return;
		}
		GazeDirectionComputer gdc = getGazeDirectionComputer();
		if ((myRawFrameProcessor != null) && (gdc != null)
					&& (SightHypothesis.getFaceNoticeConfig() != null)) {
			//  FaceObservations with same timestamp must be of different faces.
			for (Rectangle r : faceRects) {
				noticeFaceRectangle(r, videoTimestamp, cameraCenterPosEstimate);
			}
		}
		// trimAndCollapse() is also done in the videoFrameTick handler, but we would like
		// to trim the newly appeared hypos immediately.
		collapseHypos();
		// theLogger.info("facesSeen()  + complete at " + TimeUtils.currentTimeMillis() + ", used " + TimeUtils.getStampAgeSec(nowStamp) + " sec.");

	}
	private void noticeFaceRectangle(Rectangle r, long timestamp, Frame cameraCenterPosEstimate) {
		GazeDirectionComputer gdc = getGazeDirectionComputer();
		EgocentricDirection egoDir = gdc.computeGazeDirection(cameraCenterPosEstimate, r);
		TargetObjectStateEstimate tose = new TargetObjectStateEstimate(myPositionEstimator, gdc, r, timestamp);
		FaceObservation fobs = new FaceObservation();
		tose.temporarilyHackedSightObservation = fobs;
		fobs.myTOSE = tose;
		fobs.setBoundRect(r);

		long imageGrabStartStamp = TimeUtils.currentTimeMillis();

		boolean imageIsFlipped = true;
		//  TODO:  Make expansion size configurable.
		// Current:  Expand by 25% on each side.
		Rectangle expandedFaceRect = getObjectImageGrabRect(r, imageIsFlipped, IMAGE_EXPANSION_MARGIN_RATIO);
		// This causes image to be snapped from last raw-frame
		// received, which should be the correct frame given
		// order of event delivery to the native observers.
		// 1. RawFrame,   2. Detected FaceRects
		PortableImage expandedFaceImage = myRawFrameProcessor.getPortableSubImage(expandedFaceRect, imageIsFlipped);

		fobs.setFaceImage(expandedFaceImage);
		// Hack to get this image displaed in "faceq" tab
		SignalStation.getSignalStation().logDetectedObjectImage(expandedFaceImage);
		long imageGrabEndStamp = TimeUtils.currentTimeMillis();
		long imageGrabDur = imageGrabEndStamp - imageGrabStartStamp;
		fobs.setTimeStampMsec(timestamp);
		fobs.setServoSnapshot(cameraCenterPosEstimate);
		fobs.setCenterDirection(egoDir);

		Rectangle shirtSampleRect = fobs.getShirtSampleRect(getFieldBoundsRect());
		if (shirtSampleRect != null) {
			Rectangle shirtGrabRect = getObjectImageGrabRect(shirtSampleRect, imageIsFlipped, 0.0);
			PortableImage shirtImage = myRawFrameProcessor.getPortableSubImage(shirtGrabRect, imageIsFlipped);
			fobs.setShirtSampleImage(shirtImage);
		}

		// A new observation always triggers a new hypothesis, however, this
		// hypothesis is often quickly collapsed into another existing hypo.
		makeHypothesis(fobs);
		long postHypoStamp = TimeUtils.currentTimeMillis();
		long hypoConstDur = postHypoStamp - imageGrabEndStamp;
		theLogger.fine("Started imageGrab at " + imageGrabStartStamp + ", and used: " +
				imageGrabDur + " msec, then used " + hypoConstDur +
				" more msec to construct hypo ");

	}
	protected synchronized void trimAndCollapseHypos() {
		long trimAndCollapseStartStamp = TimeUtils.currentTimeMillis();
		trimHypos();
		collapseHypos();
		long endTimestamp = TimeUtils.currentTimeMillis();
		long trimAndCollapseDur = endTimestamp - trimAndCollapseStartStamp;
		theLogger.fine("trim and collapse used " + trimAndCollapseDur + " msec.");
	}
	private synchronized void trimHypos() {
		FaceNoticeConfig fnc = SightHypothesis.getFaceNoticeConfig();
		if (fnc != null) {
			int obsToKeepPerHypo = fnc.obsRetainedInHypo;
			// This trims old observations from each hypo, but keeps all those which are
			// freckle-recognized.
			trimHypothesisTails(obsToKeepPerHypo);
		}
	}
	private synchronized void collapseHypos() {
		FaceNoticeConfig fnc = SightHypothesis.getFaceNoticeConfig();
		if (fnc != null) {
			double maxCollapseDistance = fnc.mergeThresholdCogDist;
			collapseHyposUntilDone(maxCollapseDistance);
		}
	}
	public synchronized FreckleMatchBatch getFreckleMatchBatch() {
		// TODO:  Consider adjusting these params adaptively based on various factors.
		// Millisec to assume a freckle match is "still good", ignoring subsequent observations
		FreckleMatchBatch batch = new FreckleMatchBatch();
		for (SightHypothesis sh :  this.getHypoSnapshotOrderedByNum()) {
			FaceHypothesis fh = (FaceHypothesis) sh;
			FreckleMatchCandidate fmc = fh.makeFreckleMatchCandidate(HYPO_FRECKLE_MATCH_SLACK_MSEC);
			if (fmc != null) {
				batch.myCandidates.add(fmc);
			}
		}
		
		batch.mySleepMillisec = FRECKLE_BATCH_SLEEP_TIME_MSEC;
		return batch;
	}

		/* Each new observation may be associated to an existing hypothesis, or may lead
		 * to a new hypothesis.
		 * 
		 * At any time, hypotheses may be collapsed into other ones.
		 * 
		 * Hypotheses may also simply expire.
		 */

	public void trimHypothesisTails(int obsToKeepPerHypo) {
		for (SightHypothesis sh: this.getHypoSnapshotOrderedByNum()) {
			FaceHypothesis fh = (FaceHypothesis) sh;
			fh.pruneOldObsButKeepLastFreckled(obsToKeepPerHypo);
		}
	}

	protected synchronized void videoFrameTick() {
		// C++ vision processors are constructed so that this tick 
		// is delivered BEFORE the rectangles are delivered to facesSeen.
		recordPositionEstimate();
		// JointPositionSnapshot jointPosSnap = myJointSnapshotSource.getJointPositionSnapshot();
		EgocentricDirection currentCenterDir = null;
		updateAllHyposAndPruneDead(currentCenterDir);
		// invoke abstract method to consolidate our state
		trimAndCollapseHypos();
		maybeBroadcastNotifications();
		
		// This delayed job is what advances the nowCue and runs the rules engine.
		// One advantage is that during heavy system loading, the video
		// frame rate will drop, so the now duration will increase.
		// We are currently on the video callback thread, so
		// it's good to push work onto another thread.
		// BUT, we may face some problems due to that other
		// thread competing for resources (via synchronization)
		// with the work done independently/later by facesSeen (which is
		// on this same callback thread, after a reentry from the C++ layer).
		SignalStation.getSignalStation().workHardLater(this);
		// theLogger.info("videoFrameTick complete at " + TimeUtils.currentTimeMillis());
	}
	
	// Used to catch video timing signals so we can decay hypotheses
	public  class RawFrameObserver implements Observer {
		public RawFrameObserver(Observable o) {
			o.addObserver(this);
		}
		public void update (Observable o, Object arg) {
			// theLogger.fine("VMCI: update called");
			videoFrameTick(); 
		}
	}
	public synchronized FaceHypothesis makeHypothesis(FaceObservation fobs) {
		FaceHypothesis hypo = new FaceHypothesis(this, fobs);
		registerHypo(hypo);
		// beansBinding is not robust enough to handle this
		// myObservableHypotheses.add(hypo);
		return hypo;
	}

	public FaceHypothesis getFaceForNumber(Integer faceNumber) {
		return (FaceHypothesis) this.getHypoForNumber(faceNumber);
	}
	public List<FaceHypothesis> getActiveFaceHyposOrderedByNum() {
		return getFaceHyposOrderedByNumWithStatus(
				SightHypothesis.getActiveStatusList());
	}
	public List<FaceHypothesis> getRetiredFaceHyposOrderedByNum() {
		return getFaceHyposOrderedByNumWithStatus(
				CollectionUtils.list(ActivationStatus.RETIRED));
	}

	public List<FaceHypothesis> getFaceHyposOrderedByNumWithStatus(List<ActivationStatus> statusList) {
		List<FaceHypothesis> result = getHypoSnapshotOrderedByNum();
		List<FaceHypothesis> activeResult = new ArrayList<FaceHypothesis>();
		for(FaceHypothesis fh : result){
			if(statusList.contains(fh.getActivationStatus()))
				activeResult.add(fh);
		}
		return activeResult;
	}
	public FaceHypothesis getLargestActiveFace() {
		List<FaceHypothesis> hypos = getActiveFaceHyposOrderedByNum();
		Double largestDiameter = 0.0;
		FaceHypothesis largestFace = null;
		for (FaceHypothesis fh: hypos) {
			Double diam = fh.getDiameterPixels();
			if (diam > largestDiameter) {
				largestDiameter = diam;
				largestFace = fh;
			}
		}
		return largestFace;
	}
	public void setFreckleMatcher(FreckleMatcher fm) {
		myFreckleMatcher = fm;
	}
	public FreckleMatcher getFreckleMatcher() {
		return myFreckleMatcher;
	}
	
}
