/*
 *  Copyright 2008 Hanson Robotics Inc.
 *  All Rights Reserved.
 */

package org.friendularity.app.face;

import java.util.logging.Logger;
import org.cogchar.integroid.broker.IntegroidFacade;

/**
 *
 * @author Stu Baurmann
 */
public class FaceHelpFuncsMatt extends FaceHelpFuncsBase {
	private static Logger	theLogger = Logger.getLogger(FaceHelpFuncsMatt.class.getName());
	private static double	faceThresh = 1.3;

	/*
	private static void rememberPersonLocation(IntegroidFacade igf, PersonCue pc, FaceHypothesis hypo){
		if(pc == null || hypo == null){
			return;
		}
		EgocentricDirection centerFoV = igf.getAnimoidFacade().getCurrentEgocentricDirection();
		EgocentricDirection faceLocation = hypo.getEstimatedDirection();
		if(centerFoV == null || faceLocation == null){
			return;
		}
		Double dAz = centerFoV.getAzimuth().getDegrees() - faceLocation.getAzimuth().getDegrees();
		Double dEl = centerFoV.getElevation().getDegrees() - faceLocation.getElevation().getDegrees();
		Double dist = Math.sqrt(dAz*dAz + dEl*dEl);
		boolean takeJointSanpshot = (dist <= faceThresh);
		pc.rememberLocation(hypo, takeJointSanpshot);
	}
	*/

	private static void cleanUpRetiredHypos(IntegroidFacade igf){
		/*
		FaceModel fm = getFaceModel(igf);
		if(fm == null){
			return;
		}
		List<FaceHypothesis> retiredHypos = fm.getRetiredFaceHyposOrderedByNum();
		for(FaceHypothesis fh : retiredHypos){
			PersonCue pc = IntegroidHelpFuncs.findPersonForFaceNum(igf, fh.getHypothesisNumber());
			if(pc == null){ 
				fh.setActivationStatus(ActivationStatus.DEAD);
			}
		}
		 */
	}

}
