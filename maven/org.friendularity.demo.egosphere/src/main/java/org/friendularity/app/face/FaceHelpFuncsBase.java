/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.friendularity.app.face;

import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;
import org.cogchar.animoid.broker.AnimoidFacade;
import org.cogchar.integroid.broker.IntegroidFacade;
import org.freckler.sight.impl.hypo.AnimoidSightFacade;

/**
 *
 * @author Stu Baurmann
 */
public class FaceHelpFuncsBase {
	private static Logger	theLogger = Logger.getLogger(FaceHelpFuncsBase.class.getName());


	public static FaceHypothesis largestActiveFaceHypo(IntegroidFacade igf) {
		FaceHypothesis largestHypo = null;
		FaceModel fm = getFaceModel(igf);
		if (fm != null) {
			largestHypo = fm.getLargestActiveFace();
		}
		return largestHypo;
	}
	public static Integer largestActiveFaceNum(IntegroidFacade igf) {
		Integer result = null;
		FaceHypothesis lfh = largestActiveFaceHypo(igf);
		if (lfh != null) {
			result = lfh.getHypothesisNumber();
		}
		return result;
	}
	public static FaceModel getFaceModel(IntegroidFacade igf) {
		FaceModel fm = null;
		AnimoidFacade af = igf.getAnimoidFacade();
		AnimoidSightFacade asf = (AnimoidSightFacade) af;
		if (af != null) {
			fm = (FaceModel) asf.getSightModel();
		}
		return fm;
	}
	public static FaceHypothesis faceHypoForNumber(IntegroidFacade igf, Integer num) {
		FaceHypothesis result = null;
		FaceModel fm = getFaceModel(igf);
		if (num != null && fm != null) { 
			result = fm.getFaceForNumber(num);
		}
		return result;
	}
	public static List<FaceHypothesis> getAllHyposMatchingFreckleID(IntegroidFacade igf,
				String freckleID) {
		List<FaceHypothesis> resultList = new ArrayList<FaceHypothesis>();
		FaceModel fm = getFaceModel(igf);
		List<FaceHypothesis> activeHypoList = fm.getActiveFaceHyposOrderedByNum();
		for (FaceHypothesis fh : activeHypoList) {
			String fhfid = fh.getLikelyFriendPermCueID();
			if ((fhfid != null) && (fhfid.equals(freckleID))) {
				resultList.add(fh);
			}
		}
		return resultList;
	}
	public static FaceHypothesis getMostRecentlyMatchedHypoForFreckleID(IntegroidFacade igf,
				String freckleID) {
		List<FaceHypothesis> matchingHypos =  getAllHyposMatchingFreckleID(igf, freckleID);
		FaceHypothesis mrmHypo = null;
		long mrmTimestamp = 0;
		for (FaceHypothesis fh : matchingHypos) {
			FaceObservation latestFFO = fh.findLatestFreckleMatchedObservation();
			long lffoTS = latestFFO.getTimeStampMsec();
			if ((mrmHypo == null) || (lffoTS > mrmTimestamp)) {
				mrmHypo = fh;
				mrmTimestamp = lffoTS;
			}
		}
		return mrmHypo;
	}

}
