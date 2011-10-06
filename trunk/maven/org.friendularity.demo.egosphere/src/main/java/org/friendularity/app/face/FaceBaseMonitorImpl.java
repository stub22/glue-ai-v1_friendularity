/*
 *  Copyright 2008 Hanson Robotics Inc.
 *  All Rights Reserved.
 */

package org.friendularity.app.face;

import org.friendularity.app.freckle.FreckleFace;
import org.friendularity.app.person.PersonHelpFuncs;
import java.util.Observer;
import java.util.logging.Logger;
import org.cogchar.integroid.boot.SubsystemImpl;
import org.cogchar.integroid.broker.IntegroidCueBroker;
import org.cogchar.integroid.broker.IntegroidFacade;
import org.cogchar.integroid.broker.IntegroidJobBroker;
import org.cogchar.integroid.cue.PersonCue;
import org.cogchar.platform.stub.ThalamusBrokerStub;

/**
 *
 * @author Stu Baurmann
 */
public abstract class FaceBaseMonitorImpl extends SubsystemImpl implements Observer {
	private static Logger	theLogger = Logger.getLogger(FaceBaseMonitorImpl.class.getName());

	private	 boolean myInitializedFlag = false;
	private		IntegroidJobBroker		myIJB;
	private		IntegroidCueBroker		myICB;
	private		IntegroidFacade			myIGF;
	private		FaceModel				myFaceModel;

	public void ensureInitialized() {
		if (!myInitializedFlag) {
			/*
			ThalamusMonitorImpl tmi = (ThalamusMonitorImpl) lookupSubsystem(ThalamusMonitorImpl.class);
			if (tmi != null) {
				myIJB = tmi.getJobBroker();
				myICB = tmi.getCueBroker();
				myIGF  = tmi.getIntegroidFacade();
				if ((myIJB != null) && (myICB != null) && (myIGF != null)) {
					myInitializedFlag = true;
				}
			}
			 * 
			 */
		}
	}
	public ThalamusBrokerStub getFactSourceBroker() { 
		ensureInitialized();
		return myICB;
	}
	public FaceModel getFaceModel() {
		if (myFaceModel == null) {
			ensureInitialized();
			if (myInitializedFlag) {
				myFaceModel = FaceHelpFuncsStu.getFaceModel(myIGF);
				if (myFaceModel != null) {
					myFaceModel.registerObserver(this);
				}
			}
		}
		return myFaceModel;
	}
	public IntegroidFacade getIntegroidFacade() {
		ensureInitialized();
		return myIGF;
	}

	public synchronized FreckleFace getFreckleFaceForPersonCue(PersonCue pcue) {
		FreckleFace fface = null;
		if (pcue != null) {
			String freckleID = pcue.getPermPersonID();
			if (freckleID != null) {
				IntegroidFacade igf = getIntegroidFacade();
				fface = FaceHelpFuncsStu.getFreckleFaceForID(igf, freckleID);
			}
		}
		return fface;
	}
	public synchronized FaceHypothesis getFaceHypoForPersonCue(PersonCue pcue) {
		FaceHypothesis fhypo = PersonHelpFuncs.getFaceHypoForPersonCue(myIGF, pcue);
		return fhypo;
	}
}
