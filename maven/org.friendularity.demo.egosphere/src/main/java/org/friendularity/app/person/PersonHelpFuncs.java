/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.friendularity.app.person;

import org.friendularity.app.face.FaceHelpFuncsStu;
import org.friendularity.app.face.FaceHypothesis;
import org.friendularity.app.jmxwrap.SignalStation;

import java.util.logging.Logger;
import org.cogchar.integroid.awareness.AwarenessHelpFuncs;
import org.cogchar.integroid.broker.IntegroidFacade;
import org.cogchar.integroid.cue.FriendCue;
import org.cogchar.integroid.cue.PersonCue;
import org.cogchar.integroid.cue.PersonCue.NameSource;
import org.freckler.extra.FreckbaseFacade;

/**
 *
 * @author Stu Baurmann
 */
public class PersonHelpFuncs extends AwarenessHelpFuncs {
	private static Logger	theLogger = Logger.getLogger(PersonHelpFuncs.class.getName());

    public static FaceHypothesis findHypoForPerson(IntegroidFacade igf, PersonCue pc){
		PersonResolver pr = getPersonResolver(igf);
		PersonTracker pt = pr.getTrackerForPersonCueSID(pc.fetchSessionCueID());
        if(pt != null){
            return pt.getHypothesis();
        }
        return null;
    }

	public static PersonResolver getPersonResolver(IntegroidFacade igf) {
		return SignalStation.getSignalStation().myPersonResolver;
	}
	public static PersonTracker getPersonTrackerForHypo(IntegroidFacade igf, FaceHypothesis fh) {
		PersonResolver pr = getPersonResolver(igf);
		PersonTracker pt = pr.getTrackerForHypo(fh);
		return pt;
	}
	public static FaceHypothesis getFaceHypoForPersonCue(IntegroidFacade igf,
				PersonCue pcue)	 {
		PersonResolver pr = getPersonResolver(igf);
		Integer cueSID = pcue.fetchSessionCueID();
		PersonTracker pt = pr.getTrackerForPersonCueSID(cueSID);
		return pt.getHypothesis();
	}
	public static void setPersonNameFromHeardName(IntegroidFacade igf, 
				PersonCue pc, String heardName) {
		pc.setSpokenNameAndSource(heardName, PersonCue.NameSource.HEARD);
		FaceHelpFuncsStu.updateNameForFreckleFaceID(igf, pc.getPermPersonID(), heardName);
	}
	public static void syncFriendData(IntegroidFacade igf, FriendCue fc) {
		String permFriendID = fc.getPermPersonID();
		theLogger.info("Syncing friend with ID=" + permFriendID);
		// Strip leading "fbf_"
		String idTail = permFriendID.substring(4);
		theLogger.info("extracted friend ID-tail: " + idTail);
		Long friendID = Long.parseLong(idTail);
		theLogger.info("Reading freckbase friend with ID: " + friendID);
		FreckbaseFacade freckFacade = SignalStation.getSignalStation().getFreckbaseFacade();
		org.cogchar.freckbase.Friend fbFriend = freckFacade.readFreckbaseFriend(friendID);
		if (fbFriend != null) {
			syncFriendDataFromFreckbase(fc, fbFriend);
		} else {
			theLogger.warning("Could not find friend for freckbase-friend-id: " + friendID);
		}
	}

 	private static void syncFriendDataFromFreckbase(FriendCue fc,
				org.cogchar.freckbase.Friend fbf) {
		String fbFriendName = fbf.myPersonName().getOrElse(null);
		if (fbFriendName != null) {
			String oldCueName = fc.getSpokenName();
			// TODO :  convert case, soundex match?
			if ((oldCueName == null) || (!fbFriendName.equals(oldCueName))) {
				theLogger.info("Replacing old cue friendName " + oldCueName + " with freckbase name: " + fbFriendName);
				fc.setSpokenNameAndSource(fbFriendName, NameSource.REMEMBERED);
			} else {
				theLogger.info("Ignoring friendName " + fbFriendName + ", already set on cue");
			}
		} else {
			theLogger.info("Freckbase friendName is null - ignoring.");
		}
	}
	public static void syncPersonData(IntegroidFacade igf, PersonCue personCue) {
		if ((personCue != null) && (personCue instanceof FriendCue)) {
			FriendCue friendCue = (FriendCue) personCue;
			syncFriendData(igf, friendCue);
		}
	}
}
