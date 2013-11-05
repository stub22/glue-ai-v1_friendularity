/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.friendularity.app.face;

import org.friendularity.app.freckle.FreckleFace;

import org.cogchar.api.freckler.protocol.FaceRecognitionStatus;
import org.cogchar.platform.util.CollectionFilter;
import org.cogchar.sight.obs.SightObservationLog;

/**
 *
 * @author Stu Baurmann
 */
public class FreckledObsLog extends SightObservationLog<FaceObservation> {
	private		FreckleFace		myFreckleFace;
	public FreckledObsLog(FaceObservation initialObs) {
		super(initialObs);
		myFreckleFace = initialObs.getFreckleFace();
	}
	public void pruneObsButKeepEnrolls(int obsToKeep) {
		pruneOldObservations(obsToKeep, new CollectionFilter.Predicate<FaceObservation>() {
			@Override public boolean test(FaceObservation fo) {
				return (fo.getRecognitionStatus() == FaceRecognitionStatus.ENROLLED);
			}
		});
	}
}
