/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.friendularity.app.person;

import org.cogchar.api.integroid.cue.BogeyCue;
import org.cogchar.api.integroid.cue.FriendCue;
import org.cogchar.api.integroid.cue.PersonCue;
import org.cogchar.api.sight.SightAttentionStatus;
import org.cogchar.platform.stub.CueBrokerStub;
import org.cogchar.integroid.awareness.AwarenessHelpFuncs;
import org.cogchar.integroid.broker.IntegroidCueBroker;
import org.cogchar.integroid.broker.IntegroidFacade;
import org.cogchar.sight.track.SightTracker;
import org.friendularity.app.face.FaceHypothesis;
import org.friendularity.app.face.FaceModel;
import org.friendularity.app.face.FaceObservation;

import java.util.logging.Logger;

import static org.cogchar.api.sight.SightAttentionStatus.*;

/**
 * @author Stu Baurmann
 */
public class PersonTracker extends SightTracker<FaceObservation, FaceHypothesis, PersonCue> { //,  PropertyChangeListener {
	private static Logger	theLogger = Logger.getLogger(PersonTracker.class.getName());

	private		PersonResolver			myResolver;
	public static double		SCAN_SEC_UNTIL_MISSING = 1.0;

	public PersonTracker (PersonResolver r) {
		myResolver = r;
	}
	public synchronized boolean maybeDie(IntegroidCueBroker icb) {
		// To stay alive, we must have at least one of:
		// a) a live hypo
		// b) a cue-with-permID
		if (getHypothesis() == null) {
			String permID = getLikelyPermCueID();
			if (permID == null) {
				// The tracker and cue must both die!
				die(icb);
				return true;
			}
		}
		return false;
	}
	@Override public void die(CueBrokerStub cb) {
		// If this is the gazeTarget tracker, then the resolver will notice that
		// this tracker is gone, in propagateAwarenessChoiceToAttentionTarget,
		// and will set the gazeTarget to null.
		// BUT, that doesn't fix the awareness cue.
		PersonCue meCue = getCue();
		if (meCue != null) {
			IntegroidFacade igf = myResolver.getIntegroidFacade();
			AwarenessHelpFuncs.clearFocus(igf, meCue);
		}
		super.die(cb);
		myResolver = null;
	}

	// Return "true" if absorbtion successful and other tracker may die.
	// Assumption is th
	public boolean absorbTrackerWithEqualFreckleEstimate(PersonTracker other) {
		/*
		if (other.getLockedFlag()) {
			throw new RuntimeException("Cannot absorb locked tracker: " + other);
		}
		 */
		
		PersonCue meCue = getCue();
		if (meCue == null) {
			throw new RuntimeException("Tracker with null cue [" + this + " was asked to absorb another tracker: " + other);
		}
		PersonCue otherCue = getCue();
		if (otherCue != null) {
			IntegroidFacade igf = myResolver.getIntegroidFacade();
			if (AwarenessHelpFuncs.isAwarenessFocusedOnPerson(igf, otherCue)) {
				theLogger.info("Transfering focus prior to tracker/cue absorbtion");
				AwarenessHelpFuncs.transferFocus(igf, otherCue, meCue);
			}
			meCue.consume(otherCue);
		} else {
			throw new RuntimeException("Tracker [" + this + "] was asked to absorb tracker with null cue: " + other);
		}
		FaceHypothesis meHypo = getHypothesis();
		FaceHypothesis ofh = other.getHypothesis();
		if (meHypo != null) {
			if (!meHypo.hasEqualOrBetterClaimToFreckleThan(ofh)) {
				theLogger.finer("Replacing existing hypo " + meHypo + " with absorbed hypo: " + ofh);
				setHypothesis(ofh);
			} else {
				theLogger.finer("Keeping existing hypo " + meHypo + ", and discarding absorbed hypo: " + ofh);
			}
		} else {
			setHypothesis(ofh);
		}
		return true;
	}


	public String getLikelyPermCueID() {
		String result = getCuePermPersonID();
		if (result == null) {
			result = getHypoAssociatedPermCueID();
		}
		return result;
	}
	public String getHypoAssociatedPermCueID() {
		String result = null;
		FaceHypothesis meHypo = getHypothesis();
		if (meHypo != null) {
			result = meHypo.getLikelyFriendPermCueID();
		}
		return result;
	}
	public String getCuePermPersonID() {
		String result = null;
		PersonCue meCue = getCue();
		if (meCue != null) {
			result = meCue.getPermPersonID();
		}
		return result;
	}
	public void checkIdentConsistency() {
		
	}
	FaceObservation getMostRecentObsLinkedToPermFriendID() {
		PersonCue meCue = getCue();
		if (meCue != null) {
			String permPersonID = meCue.getPermPersonID();
		}
		return null;
	}
	public FaceObservation getLatestFreckledObservation() {
		FaceObservation lfo = getMostRecentObsLinkedToPermFriendID();
		if (lfo == null) {
			FaceHypothesis meHypo = getHypothesis();
			if (meHypo != null) {
				lfo = meHypo.getLatestFreckleMatchedObservation();
			}
		}
		return lfo;
	}
	public void makeFriendCueIfNeeded(IntegroidCueBroker icb) {
		PersonCue mePrevCue = getCue();
		FaceHypothesis meHypo = getHypothesis();
		if ((mePrevCue == null) || (!(mePrevCue instanceof FriendCue))) {
			theLogger.info("**************Making Friend Cue************");
			FriendCue friendCue = new FriendCue(); // (hypo);
			// double cueStrength = hypo.getStrength();
			double cueStrength = meHypo.getStrength();
			icb.registerPersonCue(friendCue, cueStrength);
			String permID = meHypo.getLikelyFriendPermCueID();
			
			if (permID != null) {
				FaceObservation lfo = meHypo.getLatestFreckleMatchedObservation();
				friendCue.setOrConfirmPermPersonID(permID, lfo.getTimeStampMsec());
				setCue(friendCue);
				theLogger.info("New friendCue: " + friendCue);
				if (mePrevCue != null) {
					theLogger.info("Clearing old non-friend[bogey?] cue" + mePrevCue);
					icb.clearCue(mePrevCue);
				}
			} else {
				throw new RuntimeException("Asked to create personCue for tracker with no friendCueID: " + this);
			}
		}
	}
	public void makeBogeyCueIfNeeded(IntegroidCueBroker icb) {
		PersonCue mePrevCue = getCue();
		FaceHypothesis meHypo = getHypothesis();
		if (mePrevCue == null) {
			if (meHypo == null) {
				throw new RuntimeException("Both prevCue and hypo are null in tracker: " + this);
			}
			String hypoTag = meHypo.getTagValue();
			BogeyCue bogeyCue = new BogeyCue(hypoTag);
			double cueStrength = meHypo.getStrength();
			icb.registerPersonCue(bogeyCue, cueStrength);
			setCue(bogeyCue);
			theLogger.info("New bogeyCue: " + bogeyCue);
		} else {
			if (!(mePrevCue instanceof BogeyCue)) {
				throw new RuntimeException("Asked to create BogeyCue but nonBogeyCue already attached to tracker: " + this);
			}
		}
	}


	@Override public String toString() {
		return "PersonTracker[" + shortDesc() 
			+ ", cuePerm=" + getCuePermPersonID()
			+ ", hypoPerm=" + getHypoAssociatedPermCueID()
			+ " [" + super.toString() + "]]";
/*			if(ed.getAzimuth() != null){
				az = ed.getAzimuth().getDegreesText();}
			if(ed.getElevation() != null){
				el = ed.getElevation().getDegreesText();}
			lastSnapshot = String.valueOf(myLastGazeSnapshot.getSnapshotTimestamp());

 */
	}
	public void updateAttentionStatus(FaceModel fm) {
		// theLogger.info("Updating attentionStatus for tracker: " + this);
		// Assumes that we are the attention focus, which implies there must be a cue.
		PersonCue meCue = getCue();
		SightAttentionStatus oldStatus = meCue.getAttentionStatus();
		double statusAgeSec = meCue.getAttentionStatusAgeSec();

		// EgocentricDirection targetDir = getEstimatedDirection();
		FaceObservation posObs = getMostAccurateObservation();
		double posObsAgeSec = posObs.getAgeSec();
		// frkObs can be non-null for BogeyCues, but only briefly before they are
		// replaced or absorbed by a FriendCue.
		FaceObservation frkObs = getLatestFreckledObservation();
		Double frkObsAgeSec = null;
		if (frkObs != null) {
			frkObsAgeSec = frkObs.getAgeSec();
		}

		SightAttentionStatus nextStatus = null;
		Double exposureAgeSecObj = getExposureAgeSec();
		if (exposureAgeSecObj != null) {
			double exposureAgeSecVal = exposureAgeSecObj.doubleValue();
			// We are currently exposed (i.e. center of obs is on-screen)
			double entryMarginSec = 0.2;
			double confirmationMarginSec = 0.2;

			if (posObsAgeSec < exposureAgeSecVal + entryMarginSec) {
				// We assume the observation is current enough that the target
				// is probably onscreen, and thus at least BLURRY.
				if ((frkObsAgeSec != null) && (frkObsAgeSec < exposureAgeSecVal + confirmationMarginSec)) {
					// TODO: Verify that  frkObs has correct freckle in corner cases,
					// e.g. we just got result back on a bogey.
					nextStatus = CONFIRMED;
					notifyAttentionConfirmed();
				} else {
					// BLURRY should be the highest status a Bogey tracker can rise
					// to for a significant length of time, because if it
					// hits confirmed, it should immediately be absorbed
					// or promoted by/to a Friend cue.
					// TODO:  How should we check for MISTAKEN?
					nextStatus = BLURRY;
				}
			} else {
				if (exposureAgeSecVal > SCAN_SEC_UNTIL_MISSING) {
					nextStatus = MISSING;
				} else {
					nextStatus = SCANNING;
				}
			}
		} else {
			nextStatus = TRAVELING;
		}
		if (nextStatus != oldStatus) {
			meCue.setAttentionStatus(nextStatus);
		}
	}

	@Override public Double getVergenceAngle(Double defaultWidth, Double slope) {
		FaceHypothesis meHypo = getHypothesis();
		if (meHypo != null) {
			Double diam = meHypo.getDiameterPixels();
			return slope * (diam-defaultWidth);
		} else {
			// This happens when the Tracker is held as a GazeTarget after the hypo has died.
			theLogger.fine("Tracker does not have hypo, so vergence angle is 0.0.  Tracker=" + this);
			return 0.0;
		}
	}
	public String getCueShortDesc() {
		String desc = "NONE";
		PersonCue meCue = getCue();
		if (meCue != null) {
			desc = meCue.getClass().getSimpleName().substring(0, 1) + "-" + getCueSID();
		}
		return desc;
	}

}
