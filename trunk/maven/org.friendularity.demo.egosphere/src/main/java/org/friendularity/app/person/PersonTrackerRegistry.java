/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.friendularity.app.person;

import org.friendularity.app.face.FaceHypothesis;
import org.friendularity.app.face.FaceObservation;

import java.util.HashSet;
import java.util.Set;
import java.util.logging.Logger;
import org.cogchar.integroid.cue.PersonCue;
import org.cogchar.sight.track.SightTrackerRegistry;

/**
 *
 * @author Stu Baurmann
 */
public class PersonTrackerRegistry extends SightTrackerRegistry<PersonTracker, FaceObservation, FaceHypothesis, PersonCue>{
	private static Logger	theLogger = Logger.getLogger(PersonTrackerRegistry.class.getName());

	public Set<PersonTracker> getAllTrackersWithoutCuePermIDs() {
		Set<PersonTracker> allTrackers = getAllTrackers();
		HashSet<PersonTracker> trackersWithoutCPIDs = new HashSet<PersonTracker>();
		for (PersonTracker pt : allTrackers) {
			if (pt.getCuePermPersonID() == null) {
				trackersWithoutCPIDs.add(pt);
			}
		}
		return trackersWithoutCPIDs;
	}
	public Set<String> getAllTrackedPermIDs() {
		Set<PersonTracker> allTrackers = getAllTrackers();
		HashSet<String> trackedPermIDs = new HashSet<String>();
		for (PersonTracker pt : allTrackers) {
			String permID = pt.getCuePermPersonID();
			if (permID != null) {
				trackedPermIDs.add(permID);
			}
		}
		return trackedPermIDs;
	}
	public PersonTracker getTrackerForPermID(String permID) {
		Set<PersonTracker> allTrackers = getAllTrackers();
		for (PersonTracker pt : allTrackers) {
			String cpid = pt.getCuePermPersonID();
			if ((cpid != null) && (cpid.equals(permID))) {
				return pt;
			}
		}
		return null;
	}
	public Set<FaceHypothesis> getAllTrackedHypos() {
		Set<FaceHypothesis> allTrackedHypos = new HashSet<FaceHypothesis>();
		Set<PersonTracker> allTrackers = getAllTrackers();
		for (PersonTracker pt : allTrackers) {
			FaceHypothesis ptfh = pt.getHypothesis();
			if (ptfh != null) {
				allTrackedHypos.add(ptfh);
			}
		}
		return allTrackedHypos;
	}
	public PersonTracker getTrackerForHypo(FaceHypothesis fh) {
		Set<PersonTracker> allTrackers = getAllTrackers();
		for (PersonTracker pt : allTrackers) {
			FaceHypothesis ptfh = pt.getHypothesis();
			if ((ptfh != null) && (ptfh.equals(fh))) {
				return pt;
			}
		}
		return null;
	}

	public PersonTracker getTrackerForPersonCueSID(Integer pcsid) {
		Set<PersonTracker> allTrackers = getAllTrackers();
		for (PersonTracker pt : allTrackers) {
			Integer ptcsid = pt.getCueSID();
			if ((ptcsid != null) && (pcsid.equals(ptcsid))) {
				return pt;
			}
		}
		return null;
	}
}
