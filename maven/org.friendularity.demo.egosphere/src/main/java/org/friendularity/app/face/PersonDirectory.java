/*
 *  Copyright 2008 Hanson Robotics Inc.
 *  All Rights Reserved.
 */

package org.friendularity.app.face;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.cogchar.integroid.cue.PersonCue;

/**
 * @author Stu Baurmann
 */
public class PersonDirectory {
	/* A hypo can be associated to at most one person (but how is this enforced?)
	 * A hypo can be assoc. to at most one freckle.
	 * A person can be assoc. to at most one hypo.
	 * A person can be assoc. to at most one freckle.
	 * We can't easily ensure that a freckle is assoc to only one person, unless we 
	 * merge persons on both 
	 */ 
	private		Map<Integer, PersonCue>		myPersonsByFaceNumber;
	private		Map<String, PersonCue>		myPersonsByFreckleID;
	
	public static Map<String, FaceHypothesis> getVisibleFrecklesWithStrongestHypos(List<FaceHypothesis> activeHypos) {
		Map<String, FaceHypothesis> result = new HashMap<String, FaceHypothesis>();
		for (FaceHypothesis fh: activeHypos) {
			String freckleID = fh.getLikelyFriendPermCueID();
			if (freckleID != null) {
				FaceHypothesis leadingHypo = result.get(freckleID);
				if ((leadingHypo == null) || (leadingHypo.getStrength() < fh.getStrength())) {
					result.put(freckleID, fh);
				}
			}
		}
		return result;
	}
	public static List<PersonCue> findPersonsMatchingFreckleID(List<PersonCue> persons, String freckleID) {
		List<PersonCue> result = new ArrayList<PersonCue>();
		if (freckleID != null) {
			for (PersonCue pc: persons) {
				String pfid = pc.getPermPersonID();
				if ((pfid != null) && (pfid.equals(freckleID))) {
					result.add(pc);
				}
			}
		}
		return result;
	}

	/* 1) For each visible freckle k, find the strongest hypo h.
	 *	If a personCue p is associated to that freckle k already, update the hypo of p to h.
	 * Otherwise, if hypo h is already associated with some cue p, update p to associate with k.
	 * Otherwise, create a new cue p and associate to both h and k.
	 * 
	 * For all hypos wh associated with k which are weaker than h, we want there
	 * to be no person cue (which will stink if k is getting matches all over the place -
	 * we just have to see how bad this problem is in practice).  We may fix this by 
	 * (sometimes?) killing the freckle-association for the wh, so then they can drop into 
	 * category 2 below.
	 * 
	 * 2) For each hypo which is not associated to a freckle, if it is of sufficient strength,
	 * and is not already associated with a person cue, then create a new person cue and 
	 * associate it.
	 * 
	 */ 
	
}
