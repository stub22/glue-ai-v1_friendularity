/*
 *  Copyright 2008 Hanson Robotics Inc.
 *  All Rights Reserved.
 */

package org.friendularity.app.face;


import org.friendularity.app.freckle.FreckleMatchCandidate;
import org.friendularity.app.freckle.FreckleFace;

import java.lang.Long;
import java.util.HashSet;

import java.util.Map;
import java.util.NavigableMap;

import java.util.Set;
import java.util.logging.Logger;
import org.cogchar.sight.api.facerec.FaceNoticeConfig;

import org.cogchar.sight.api.facerec.FaceRecognitionStatus;
import org.cogchar.sight.api.core.SightPort;
import org.cogchar.platform.util.CollectionFilter;
import org.freckler.sight.impl.hypo.SightHypothesis;

/**
 * @author Stu Baurmann
 */
public class FaceHypothesis extends SightHypothesis<FaceObservation> {
	private static Logger theLogger = Logger.getLogger(FaceHypothesis.class.getName());

	public static final String PROP_TIMESTAMP_MILLISEC		= "timestampMillisec";
	public static final String PROP_STRENGTH				= "strength";	

	private	FaceObservation		myCachedLatestFreckledObs;

	private	int					myPrunedFreckleMatchAttemptCount = 0;
	private	int					myPrunedFreckleMatchSuccessCount = 0;
	
	public FaceHypothesis(FaceModel fmod, FaceObservation initialObs) {
		super(fmod, initialObs);		
	}

	@Override public synchronized void absorb(SightHypothesis inferior) {
		super.absorb(inferior);
		updateLikelyFreckleFace();
	}
	@Override protected void cleanup() {
		super.cleanup();
		myCachedLatestFreckledObs = null;
	}
	@Override protected void adjustStatisticsForPrunedObs(FaceObservation pruned) {
		super.adjustStatisticsForPrunedObs(pruned);
		FaceRecognitionStatus rstat = pruned.getRecognitionStatus();
		if (rstat.attemptComplete()) {
			myPrunedFreckleMatchAttemptCount++;
		}
		if (rstat.matchedOrEnrolled()) {
			myPrunedFreckleMatchSuccessCount++;
		}
	}
	protected FaceModel getFaceModel() {
		return (FaceModel) getSightModel();
	}

	public synchronized FreckleMatchCandidate  makeFreckleMatchCandidate(Long slackMsec) {
		FreckleMatchCandidate fmc = null;
		// TODO:  Prioritize the "mostAccurate" observation [taking into account motion uncertainty]
		FaceObservation nextToTry = getLatestFreckleUntriedObservation();
		if (nextToTry != null) {
			FaceObservation	latestFreckledObs = findLatestFreckleMatchedObservation();
			// We try to match the latest untried observation that is more recent than our last successful match,
			// as long as it's at least slackMsec newer than our last successful match.
			// Assertion: Successful matches take sig. longer than "eyes not found" failures - true?
			
			// TODO:  Add freckleStrength as an ingredient in the decision.
			boolean okToMatch = false;
			if (latestFreckledObs == null)  {
				okToMatch = true;
			} else {
				Long nextToTryStampMsec = nextToTry.getTimeStampMsec();
				Long latestStampMsec = latestFreckledObs.getTimeStampMsec();
				Long cutoffStampMsec = latestStampMsec + slackMsec;
				theLogger.finest("nextToTryStamp=" + nextToTryStampMsec + ", latestStamp=" + latestStampMsec
						+ ", slack=" + slackMsec + ", cutoff=" + cutoffStampMsec);
				if ( nextToTryStampMsec > cutoffStampMsec) {
					okToMatch = true;
				} 				
			}
			theLogger.finer("hypo " + this.getHypothesisNumber() + " - attemptMatch?: " + okToMatch);
			if (okToMatch) {
				fmc = new FreckleMatchCandidate();
				fmc.setHypothesis(this);
				fmc.setObservationToTry(nextToTry);
			}
		}
		return fmc;
	}
	public int getRetainedAndFreckleMatchAttemptedCount() {
		return countRetainedTimestampsWhereObsMatchesPredicate(new CollectionFilter.Predicate<FaceObservation>() {
			public boolean test(FaceObservation fobs) {
				return fobs.getRecognitionStatus().attemptComplete();
			}
		});
	}
	public int getRetainedAndFreckleMatchedCount() {
		return countRetainedTimestampsWhereObsMatchesPredicate(new CollectionFilter.Predicate<FaceObservation>() {
			public boolean test(FaceObservation fobs) {
				return fobs.getRecognitionStatus().matchedOrEnrolled();
			}
		});
	}

	public Integer getFreckleMatchAttemptCount() {
		return myPrunedFreckleMatchAttemptCount + getRetainedAndFreckleMatchedCount();
	}
	public Integer getFreckleMatchSuccessCount() {
		return myPrunedFreckleMatchSuccessCount + getRetainedAndFreckleMatchedCount();
	}
	protected synchronized FaceObservation findLatestFreckleMatchedObservation() {
		return getMostRecentObsMatchingPredicate(new CollectionFilter.Predicate<FaceObservation>() {
			public boolean test(FaceObservation fobs) {
				Long friendID = fobs.getFriendID();
				return (friendID != null);
				// FreckleFace fface = fobs.getFreckleFace();
				// return (fface != null);
			}
		});
	}
	public synchronized FaceObservation getLatestFreckleMatchedObservation() {
		return myCachedLatestFreckledObs;
	}
	public boolean hasEqualOrBetterClaimToFreckleThan(FaceHypothesis another) {
		FaceObservation mFO = getLatestFreckleMatchedObservation();
		FaceObservation aFO = another.getLatestFreckleMatchedObservation();
		long mStamp = mFO.getTimeStampMsec();
		long aStamp = aFO.getTimeStampMsec();
		return mStamp >= aStamp;
	}
	public synchronized void updateLikelyFreckleFace() {
		myCachedLatestFreckledObs = findLatestFreckleMatchedObservation();
	}	 
	private  synchronized FreckleFace getLikelyFreckleFace() {
		FaceObservation latestFO = getLatestFreckleMatchedObservation();
		if (latestFO != null) {
			FreckleFace ff = latestFO.getFreckleFace();
			return ff;
		}  else {
			return null;
		}
	}
	public synchronized Long getLikelyFriendID() {
		FaceObservation latestFO = getLatestFreckleMatchedObservation();
		if (latestFO != null) {
			return latestFO.getFriendID();
		} else {
			return null;
		}
	}

	public  synchronized String getLikelyFriendPermCueID() {
		Long friendID = getLikelyFriendID();
		if (friendID != null) {
			return "fbf_" + friendID.toString();
		} else {
			return null;
		}
		/*
		FreckleFace ff = getLikelyFreckleFace();
		if (ff != null) {
			return ff.getFreckleID();
		} else {
			return null;
		}
		 */
	}
	public synchronized Double getLikelyFreckleStrength() {
		Double likelyFreckleStrength = null;
		FaceObservation latestFreckleMatchedObs = getLatestFreckleMatchedObservation();
		if (latestFreckleMatchedObs != null) {
			likelyFreckleStrength = latestFreckleMatchedObs.getFreckleMatchStrength();
		}
		return likelyFreckleStrength;
	}	
	public synchronized FaceObservation getLatestFreckleUntriedObservation() {
		return getMostRecentObsMatchingPredicate(new CollectionFilter.Predicate<FaceObservation>() {
			public boolean test(FaceObservation fobs) {
				FaceRecognitionStatus rstat = fobs.getRecognitionStatus();
				return (rstat == FaceRecognitionStatus.UNTRIED);
			}
		});
	}

	
	public synchronized int pruneOldObsButKeepLastFreckled(int obsToKeep) {
		Set<Long> immuneTimestamps = null;
		FaceObservation  latestFreckleMatch = findLatestFreckleMatchedObservation();
		if (latestFreckleMatch != null) {
			immuneTimestamps = new HashSet<Long>();
			immuneTimestamps.add(latestFreckleMatch.getTimeStampMsec());
		}
		return pruneOldObservations(obsToKeep, immuneTimestamps);
	}


	public String getTagValue() {
		FreckleFace ff = getLikelyFreckleFace();
		if (ff != null) {
			return ff.getDescription();
		} else {
			return "GHOST-" + getUniqueID();
		}
	}
	public Double getDiameterPixels() {
		FaceObservation mrObs = (FaceObservation) getMostRecentObservation();
		Double obsPixelDiameter = mrObs.getDiameterPixels();
		return obsPixelDiameter;
	}
	public Double getDiameterDegrees() {
		FaceModel fm = getFaceModel();
		SightPort vp = fm.getGazeDirectionComputer().getViewPort();
		FaceObservation mrObs = (FaceObservation) getMostRecentObservation();
		Double obsDiamDeg = mrObs.getDiameterDeg(vp);
		return obsDiamDeg;
	}



	public String toString() {
		return "FaceHypothesis[num=" + getHypothesisNumber() + ", tag=" + getTagValue() + "]";
	}

	@Override public double computeAdditionalDistance(SightHypothesis sh) {
		double addSum = 0.0;
		FaceNoticeConfig fnc = SightHypothesis.getFaceNoticeConfig();
		FaceHypothesis other = (FaceHypothesis) sh;

		String fid = getLikelyFriendPermCueID();
		String ofid = other.getLikelyFriendPermCueID();
		if ((fid != null) && (ofid != null)) {
			if (fid.equals(ofid)) {
				theLogger.info("Adding " + fnc.cogDistCoeffFreckleMatch + " to cogDist between "
					+ getHypothesisNumber() + " and " + other.getHypothesisNumber()
					+ " because hypo freckles are equal[" + fid + "]");
				addSum += fnc.cogDistCoeffFreckleMatch;
			} else {
				addSum += fnc.cogDistCoeffFreckleMismatch;
			}
		}
		return addSum;
	}
	public FaceHypothesis forkObsSinceLastFreckledIntoNewHypo() {
		FaceHypothesis forkedHypo = null;
		FaceObservation freckled = getLatestFreckleMatchedObservation();
		Long freckledTS = freckled.getTimeStampMsec();
		// This map is backed by the hypo's collection, so removing entries
		// from it removes them from the collection.
		NavigableMap<Long, FaceObservation> obsToFork = getObsNewerThanStamp(freckledTS,
					false);
		theLogger.info("Forking hypo " + getHypothesisNumber() + " after timestamp " + freckledTS
					+ " to create a new hypo with " + obsToFork.size() + " observations");
		// pollFirstEntry removes and returns first entry.
		Map.Entry<Long, FaceObservation> firstEntry = obsToFork.pollFirstEntry();
		if (firstEntry != null) {
			FaceObservation firstForkedObs = firstEntry.getValue();
			obsToFork.remove(firstEntry.getKey());
			forkedHypo = new FaceHypothesis(getFaceModel(), firstForkedObs);
			// Both existing and forked continue at current strength.
			forkedHypo.updateStrength(getStrength());
			getFaceModel().registerHypo(forkedHypo);
			firstEntry = obsToFork.pollFirstEntry();
			while (firstEntry != null) {
				forkedHypo.insertObservation(firstEntry.getValue());
				firstEntry = obsToFork.pollFirstEntry();
			}
		}
		// TODO: Fix freckle-attempt counts
		return forkedHypo;
	}
}
