/*
 *  Copyright 2008 Hanson Robotics Inc.
 *  All Rights Reserved.
 */

package org.friendularity.app.freckle;


import org.friendularity.app.face.FaceHypothesis;
import org.friendularity.app.face.FaceObservation;
import org.friendularity.app.jmxwrap.SignalStation;


import java.util.logging.Logger;
import org.cogchar.sight.api.facerec.FaceRecognitionStatus;
import org.cogchar.sight.api.facerec.FreckleResult;
import org.cogchar.sight.api.facerec.FreckleMatchConfig;
import org.cogchar.sight.api.facerec.FreckleQuery;
import org.cogchar.sight.api.obs.PortableImage;



import org.freckler.extra.FreckbaseFacade;

/**
 *
 * @author Stu Baurmann
 */
public class FreckleMatchCandidate {
	private static Logger theLogger = Logger.getLogger(FreckleMatchCandidate.class.getName());
	
	private FaceHypothesis	myHypothesis;
	private FaceObservation	myObsToTry;
	private FreckleQuery	myQuery;
	// public FaceObservation	latestFreckledObs;
	// public boolean			hypoNeverMatched;
	private String			myResultFreckleID;

	public FreckleQuery makeFreckleQuery(Long freckbaseObsID, FreckleMatchConfig conf) {
		myObsToTry.verifyFreckleMatchAttemptEligibility();
		myObsToTry.setRecognitionStatus(FaceRecognitionStatus.PROCESSING);
		PortableImage pimg = myObsToTry.getFaceImage();
		String handle = getQueryHandle();
		String possibleNewFreckleID = myHypothesis.getUniqueID() + "-" + handle;
		myQuery = new FreckleQuery(handle, pimg, possibleNewFreckleID, conf);
		myQuery.setFreckbaseObsID(freckbaseObsID);
		return myQuery;
	}
	public String getQueryHandle() {
		return "FMC-" + this.hashCode();
	}
	// theLogger.fine("Matched freckleFace " +matchedFace.getDescription());

	public void pullAndProcessFreckbaseResult(FreckleResult fres) {
		FreckbaseFacade freckFacade = SignalStation.getSignalStation().getFreckbaseFacade();
		Long fbObsID = myQuery.getFreckbaseObsID();
		org.cogchar.freckbase.Observation fbo = freckFacade.readFreckbaseObs(fbObsID);
		theLogger.info("Pulled Obs: " + fbo.toString());
		myObsToTry.syncFromFreckbaseObs(fbo);
	}

	public void recordFreckleResult(FreckleResult fres, FreckleFaceInventory knownFaceInventory,
				Double matchMinAcceptThresh) {
		pullAndProcessFreckbaseResult(fres);
		/*
		// If this is a newly enrolled freckle, knownFaceInventory should already
		// have been updated when this is called.
		Double enrollThresh = myQuery.getMatchScoreEnrollPreventThresh();
		// Threshold sanity check
		if ((enrollThresh != null) && (enrollThresh > matchMinAcceptThresh)) {
			throw new RuntimeException("Illegal Config: Enrollment-prevention score thresh[" + enrollThresh
					+ "] is greater than Match-acceptance thresh [" + matchMinAcceptThresh + "]");
		}
		
		// myHypothesis.incrementFreckleMatchAttemptCount();
		myObsToTry.setFreckleSampleQualitySummary(fres.fetchSampleQualitySummary());
		FreckleFace knownFace = null;
		FreckleResult.PossibleMatch bestAlt = fres.fetchBestAlternative();
		FreckleResult.EnrollmentAction enrollAction = fres.getEnrollmentAction();

		FaceRecognitionStatus status = FaceRecognitionStatus.FAILED;
		if (bestAlt != null) {
			if (bestAlt.getMatchStrength() >= matchMinAcceptThresh) {
				// Good match.  Now for a quick sanity check!
				if (enrollAction.enrollAttempted()) {
					status = FaceRecognitionStatus.ERROR;
					myObsToTry.setRecognitionStatus(status);
					throw new RuntimeException("Enrollment was attempted despite a good match: " + fres);
				}
				// The world is sane.  Proceed to accept the match.
				myResultFreckleID = bestAlt.getFreckleID();
				knownFace = knownFaceInventory.getKnownFace(myResultFreckleID);
				status = FaceRecognitionStatus.MATCH_ACCEPTED;
				myObsToTry.setFreckleMatchStrength(bestAlt.getMatchStrength());
			} else {
				theLogger.warning("Ignoring weak freckle match result on handle: " + getQueryHandle());
			}
		}
		if (enrollAction == FreckleResult.EnrollmentAction.SUCCESS) {
			status = FaceRecognitionStatus.ENROLLED;
			myResultFreckleID = fres.getEnrolledFreckleID();
			// leave "match strength" at null.
		}
		if (myResultFreckleID != null) {
			knownFace = knownFaceInventory.getKnownFace(myResultFreckleID);
		}
		if (knownFace != null) {
			String hypoPrevFID = myHypothesis.getLikelyFreckleID();
			FaceHypothesis forkedHypo = null;
			if ((hypoPrevFID != null) && !hypoPrevFID.equals(myResultFreckleID)) {
				forkedHypo = myHypothesis.forkObsSinceLastFreckledIntoNewHypo();
				// forkedHypo.incrementFreckleMatchSuccessCount();
			}
			myObsToTry.setFreckleFace(knownFace);
			knownFace.addMatchedObservation(myObsToTry);
			// myHypothesis.incrementFreckleMatchSuccessCount();
			myHypothesis.updateLikelyFreckleFace();
			if (forkedHypo != null) {
				forkedHypo.updateLikelyFreckleFace();
			}
		}
		myObsToTry.setRecognitionStatus(status);
		 */
	}
	public void setHypothesis(FaceHypothesis hypo) {
		myHypothesis = hypo;
	}
	public FaceHypothesis getHypothesis() {
		return myHypothesis;
	}
	public void setObservationToTry(FaceObservation fobs) {
		myObsToTry = fobs;
	}
	public FaceObservation getObservationToTry() {
		return myObsToTry;
	}
	public String getResultFreckleID() {
		return myResultFreckleID;
	}
	public void setResultFreckleID(String freckleID) {
		myResultFreckleID = freckleID;
	}

}
