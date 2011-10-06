/*
 *  Copyright 2008 Hanson Robotics Inc.
 *  All Rights Reserved.
 */

package org.friendularity.app.face;

import org.friendularity.app.freckle.FreckleFace;
import org.friendularity.app.freckle.FreckleMatcher;

import java.util.logging.Logger;
import org.cogchar.integroid.broker.IntegroidFacade;

/**
 *
 * @author Stu Baurmann
 */

public class FaceHelpFuncsStu extends FaceHelpFuncsBase {
	private static Logger	theLogger = Logger.getLogger(FaceHelpFuncsStu.class.getName());


/*
	public static PersonCue collapseFreckleMatchedPersonCues(IntegroidFacade igf, PersonCue pc1, 
				PersonCue pc2) {

		FaceModel fm = (FaceModel) igf.getAnimoidFacade().getSightModel();
		String freckleID = pc1.gePermPersonID();
		Integer hypoNum1 = pc1.getFaceNumber();
		Integer greetCount1 = pc1.getPersonalGreetingCount();
		Integer hypoNum2 = pc2.getFaceNumber();
		Integer greetCount2 = pc2.getPersonalGreetingCount();
		FaceHypothesis fh1 = fm.getFaceForNumber(hypoNum1);
		FaceHypothesis fh2 = fm.getFaceForNumber(hypoNum2);
		return null;
	}
*/

/*...
			String spokenName = pcue.getSpokenName();
			if (hypoAndCueFrecklesEqual && (spokenName != null) && (pcue.getNameSource() == PersonCue.NameSource.HEARD)) {
				updateNameForFreckleFaceID(igf, hypoFreckleID, spokenName);
			}
		}
*/
	public static FreckleFace getFreckleFaceForID(IntegroidFacade igf, String freckleID) {
		FaceModel faceModel = getFaceModel(igf);
		FreckleMatcher freckleMatcher = faceModel.getFreckleMatcher();
		FreckleFace freckleFace = freckleMatcher.getInventory().getKnownFace(freckleID);
		return freckleFace;
	}
	public static void updateNameForFreckleFaceID(IntegroidFacade igf, String freckleID, String name) {
		if (freckleID != null) {
			FreckleFace ff = getFreckleFaceForID(igf, freckleID);
			if (ff != null) {
				theLogger.info("Setting name for FreckleFace with ID=" + freckleID + " to " + name);
				ff.setPersonName(name);
			}
		}
	}




}
