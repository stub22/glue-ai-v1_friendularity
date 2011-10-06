/*
 *  Copyright 2008 Hanson Robotics Inc.
 *  All Rights Reserved.
 */

package org.friendularity.app.freckle;



import org.cogchar.platform.stub.PropertyChangeNotifier;
import org.friendularity.app.face.FaceObservation;
import org.friendularity.app.face.FreckledObsLog;

/**
 *
 * @author Stu Baurmann
 */
public class FreckleFace extends PropertyChangeNotifier {

	protected String	freckleID;
	protected String	personName;
	public static final String PROP_FRECKLE_ID = "freckleID";
	public static final String PROP_PERSON_NAME = "personName";	
	public static final String PROP_MATCHED_OBSERVATION_COUNT = "matchedObservationCount";

	// If not "transient", value gets persisted in freckleFace.xml
	private transient int			matchedObsCount = 0;
	// private	transient List<FaceObservation>	myMatchedObservations;
	private	transient FreckledObsLog	myLog;

	public FreckleFace() {
		completeInit();
	}
	public void completeInit() {
		super.completeInit();
	}
	/**
	 * Get the value of freckleID
	 *
	 * @return the value of freckleID
	 */
	public String getFreckleID() {
		return freckleID;
	}

	/**
	 * Set the value of freckleID
	 *
	 * @param freckleID new value of freckleID
	 */
	public void setFreckleID(String freckleID) {
		String oldFreckleID = freckleID;
		this.freckleID = freckleID;
		safelyFirePropertyChange(PROP_FRECKLE_ID, oldFreckleID, freckleID);
	}
	
	public void addMatchedObservation(FaceObservation fobs) {
		if (myLog == null) {
			myLog = new FreckledObsLog(fobs);
		} else {
			myLog.insertObservation(fobs);
		}
		int oldCount = matchedObsCount++;
		safelyFirePropertyChange(PROP_MATCHED_OBSERVATION_COUNT, oldCount, matchedObsCount);
	}
	public int getMatchedObservationCount() {
		return matchedObsCount;
	}
	public int getRetainedObservationCount() {
		if (myLog != null) {
			return myLog.getRetainedObservationCount();
		} else {
			return 0;
		}
	}
	public synchronized void trimOldObservations(int obsToKeep) {
		if(myLog != null) {
			myLog.pruneObsButKeepEnrolls(obsToKeep);
		}
	}
	public FreckledObsLog getFreckledObsLog() {
		return myLog;
	}
	public String getPersonName() {
		return personName;
	}
	public void setPersonName(String pn) {
		String oldPersonName = personName;
		personName = pn;
		safelyFirePropertyChange(PROP_PERSON_NAME, oldPersonName, personName);		
	}

	public String getDescription() {
		if (personName != null) {
			return "NAME-" + personName + "-ID-" + freckleID;
		} else {
			return "UNNAMED-ID-" + freckleID;
		}
	}
	public String toString() {
		return "FreckleFace[" + getDescription() + "]";
	}
	
}
