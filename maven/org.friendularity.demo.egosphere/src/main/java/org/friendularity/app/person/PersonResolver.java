/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.friendularity.app.person;


import org.friendularity.app.face.FaceHelpFuncsStu;
import org.friendularity.app.face.FaceHypothesis;
import org.friendularity.app.face.FaceModel;
import org.friendularity.app.face.FaceObservation;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.logging.Logger;

import org.cogchar.animoid.broker.AnimoidFacade;
import org.cogchar.animoid.config.AnimoidConfig;
import org.cogchar.animoid.gaze.IGazeTarget;
import org.cogchar.integroid.broker.IntegroidFacade;
import org.cogchar.integroid.broker.IntegroidHelpFuncs;
import org.cogchar.integroid.cue.AwarenessCue;
import org.cogchar.integroid.cue.MotionCue;
import org.cogchar.integroid.cue.PersonCue;
import org.cogchar.platform.util.TimeUtils;
import org.cogchar.sight.track.SightCue;


/**
 *
 * @author Stu Baurmann
 */
public class PersonResolver extends PersonTrackerRegistry {
	private static Logger	theLogger = Logger.getLogger(PersonResolver.class.getName());

	private IntegroidFacade				myIGF;
	private	FaceModel					myFaceModel;

	public PersonResolver(IntegroidFacade igf) {
		myIGF = igf;
		myFaceModel = FaceHelpFuncsStu.getFaceModel(igf);
	}
	public synchronized void resolveAllState(FaceModel fm) {
		long startStamp = TimeUtils.currentTimeMillis();
		reconcileTrackersAndHypos();
		promoteOrAbsorbFreckledHypoTrackers();
		updateStrengthsAndDetachMismatchedHypos();
		saveStuff();
		// We get confirmation notices during attentionStatus update.
		// But what about reconfirmation that happens without attention?
		// reconfirmPersonFreckles();
		propagateAwarenessChoicesToAttentionTarget();
		updateAttentionStatus();
		long endStamp = TimeUtils.currentTimeMillis();
		long elapsedMsec = endStamp - startStamp;
		theLogger.finer("resolveAllState() started at " + startStamp + " and used " + elapsedMsec + " msec");

	}
	private synchronized void reconcileTrackersAndHypos() {
		theLogger.finer("*************************Reconciling trackers and hypos****************");

		List<FaceHypothesis> activeHypoList = myFaceModel.getActiveFaceHyposOrderedByNum();
		if (activeHypoList == null) {
			theLogger.warning("ActiveHypoList is null - reconcile failed");
			return;
		}
		Set<FaceHypothesis> activeHypos = new HashSet<FaceHypothesis>(activeHypoList);
		Set<FaceHypothesis> hyposAlreadyTracked = getAllTrackedHypos();

		createTrackersForDanglingHypos(activeHypos, hyposAlreadyTracked);
		detachDeadHyposFromTrackers(activeHypos, hyposAlreadyTracked);
	}
	private synchronized void promoteOrAbsorbFreckledHypoTrackers() {
		theLogger.finer("*************************Resolving permanentIDs!****************");
		List<PersonTracker> tempTrackersToDetach = new ArrayList<PersonTracker>();
		Set<PersonTracker> tempTrackers = getAllTrackersWithoutCuePermIDs();
		for (PersonTracker tempPT : tempTrackers) {
			FaceObservation latestFO = tempPT.getLatestFreckledObservation();
			if (latestFO != null) {
				// PT should be either promoted or absorbed, unless it is locked
				
				// String fid = latestFO.getFreckleFace().getFreckleID();
				Long friendID = latestFO.getFriendID();
				String friendCuePermID = "fbf_" + friendID;
				theLogger.info("Promoting tracker for friendCuePermID=" + friendCuePermID);

				Long freckledObsStamp = latestFO.getTimeStampMsec();
				PersonTracker existingPT = getTrackerForPermID(friendCuePermID);
				if (existingPT != null) {
					// ept must absorb pt.
					// BUT, what if pt was in use, in particular as the target
					// of an attention job.?   Hence the "locked" flag.
				//	if (!tempPT.getLockedFlag()) {
					if (existingPT.absorbTrackerWithEqualFreckleEstimate(tempPT)) {
						tempTrackersToDetach.add(tempPT);
					} else {
						theLogger.warning("existing tracker [" + existingPT + "] could not absorb [" + tempPT + "]");
					}
				//	}
				} else {
					// pt must be promoted to a perm tracker
					tempPT.makeFriendCueIfNeeded(myIGF.getCueBroker());
					PersonCue epc = tempPT.getCue();
					theLogger.info("Confirming cueID=" + friendCuePermID + " for presumed friendCue: " + epc);
					epc.setOrConfirmPermPersonID(friendCuePermID, freckledObsStamp);
				}
			} else {
				tempPT.makeBogeyCueIfNeeded(myIGF.getCueBroker());
			}
		}
		for (PersonTracker dt : tempTrackersToDetach) {
			detachTrackerFromHypo(dt);
		}
	}

	private synchronized void updateStrengthsAndDetachMismatchedHypos() {
		AnimoidConfig aconf = myIGF.getAnimoidFacade().getAnimoidConfig();
		Set<PersonTracker> allTrackers = getAllTrackers();
		for (PersonTracker pt : allTrackers) {
			String permID = pt.getCuePermPersonID();
			FaceHypothesis pth = pt.getHypothesis();
			if (pth != null) {
				String fhfid = pth.getLikelyFriendPermCueID();
				if ((fhfid != null) && (permID != null) && (!fhfid.equals(permID))) {
					// This shouldn't happen any more, because hypos are forking themselves!!!
					// Hypo is mismatched.  Detach it, allowing it to be picked up again.
					theLogger.severe("********** Detaching mismatched hypo: " + pth + " from permTracker: " + pt);
					detachTrackerFromHypo(pt);
				}
			}
			pt.propagateStrengthAndStatus(myFaceModel, aconf);
		}
	}


	private synchronized void createTrackersForDanglingHypos(Set<FaceHypothesis> activeHypos,
					Set<FaceHypothesis> hyposAlreadyTracked) {
		Set<FaceHypothesis> hyposNeedingTracking = new HashSet<FaceHypothesis>(activeHypos);
		hyposNeedingTracking.removeAll(hyposAlreadyTracked);
		theLogger.finer("HyposNeedingTracking: " + hyposNeedingTracking);
		for (FaceHypothesis pfh : hyposNeedingTracking) {
			// We want to avoid continuously creating and killing trackers
			// for hypos with inferior freckles.

			String freckleID = pfh.getLikelyFriendPermCueID();
			if (freckleID != null) {
				FaceHypothesis bestHypoForFreckle = FaceHelpFuncsStu.getMostRecentlyMatchedHypoForFreckleID(myIGF, freckleID);
				if ((bestHypoForFreckle != null) && (bestHypoForFreckle != pfh)) {
					theLogger.info("Not creating tracker for hypo " + pfh.getHypothesisNumber() + " with old, duplicated freckleID " + freckleID);
					// Consider calling killHypothesis to put this puppy out of its misery.
					// But since they have same freckle, they should collapse unless there are overlaps.
					continue;
				}
			}
			addTrackerForHypo(pfh);
		}
	}
	private synchronized void detachDeadHyposFromTrackers(Set<FaceHypothesis> activeHypos,
					Set<FaceHypothesis> hyposAlreadyTracked) {
		Set<FaceHypothesis> invalidHypos = new HashSet<FaceHypothesis>(hyposAlreadyTracked);
		invalidHypos.removeAll(activeHypos);

		theLogger.finer("InvalidHypos: " + invalidHypos);

		for (FaceHypothesis ifh : invalidHypos) {

			PersonTracker pt = getTrackerForHypo(ifh);
			detachTrackerFromHypo(pt);
		}
	}
	private synchronized void saveStuff() {
		for (PersonTracker pt: getAllTrackers()) {
			pt.saveStuff();
		}
	}
	private synchronized PersonTracker addTrackerForHypo(FaceHypothesis hypo) {
		theLogger.info("Adding tracker for hypo: " + hypo);
		PersonTracker pt = new PersonTracker(this);
		pt.setHypothesis(hypo);
		registerTracker(pt);
		return pt;
	}
	private synchronized void detachTrackerFromHypo(PersonTracker pt) {
		theLogger.info("Detaching hypo from tracker: " + pt);
		pt.setHypothesis(null);
		// The tracker will die unless it has a permanent freckleID
		if (pt.maybeDie(myIGF.getCueBroker())) {
			unregisterTracker(pt);
		}
	}
	private void propagateAwarenessChoicesToAttentionTarget() {
		AnimoidFacade af = myIGF.getAnimoidFacade();
		IGazeTarget currTarget = af.getAttentionTarget();
		AwarenessCue ac = myIGF.getCueBroker().getAwarenessCue();
		if (ac != null) {
			PersonCue fixPC = ac.getFixationPerson();
			SightCue glanceSC = ac.getGlanceSight();
			// theLogger.severe("Possible glance target: " + glanceSC);
			SightCue focusSC = null;
			if (glanceSC != null){
				focusSC = glanceSC;
			} else {
				focusSC = fixPC;
			}
			IGazeTarget	nextTarget = null;
			if (focusSC != null) {
				if (focusSC instanceof PersonCue) {
					Integer focusSessionCueID = focusSC.fetchSessionCueID();
					nextTarget = getTrackerForPersonCueSID(focusSessionCueID);
				} else if (focusSC instanceof MotionCue) {
					nextTarget = ((MotionCue) focusSC).fetchPeakTracker();
				}
			}
			// If the previous focusTracker has died, then nextFocusTracker
			// will be null at this point, which will cause gazeTarget to be cleared.
			if (nextTarget != currTarget) {
				if ((currTarget != null) && (currTarget instanceof PersonTracker)) {
					PersonTracker prevTargetTracker = (PersonTracker) currTarget;
					prevTargetTracker.clearAttentionStatus();
				}
				theLogger.info("Changing gazeTarget from " + currTarget + " to " + nextTarget);
				IntegroidHelpFuncs.suggestGazeTarget(myIGF, nextTarget);
			} else {
				// theLogger.info("NextFocusTracker == currTarget");
			}
			/* TODO:  Also propagate gazeStrategyName, but only if changed.
			 */
		} else {
			theLogger.warning("No awareness cue");
		}
	}
	private void updateAttentionStatus() {
		AnimoidFacade af = myIGF.getAnimoidFacade();
		IGazeTarget currTarget = af.getAttentionTarget();

		if ((currTarget != null) && (currTarget instanceof PersonTracker)) {
			PersonTracker targetTracker = (PersonTracker) currTarget;
			targetTracker.updateAttentionStatus(myFaceModel);
		}
	}
	protected IntegroidFacade getIntegroidFacade() {
		return myIGF;
	}
}
