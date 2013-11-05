/*
 *  Copyright 2011 by The Cogchar Project (www.cogchar.org).
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package org.friendularity.gaze.estimate;


import java.awt.Rectangle;
import java.io.Serializable;
import org.cogchar.animoid.calc.estimate.PositionEstimator;
import org.friendularity.sight.api.freckle.FaceNoticeConfig;

import org.cogchar.api.animoid.protocol.Frame;

import org.friendularity.sight.api.core.SightRelatedStateEstimate;

/**
 * @author Stu B. <www.texpedient.com>
 */
public class TargetObjectStateEstimate extends SightRelatedStateEstimate implements Serializable {

	public Frame					jointPosAtObs;
	public Frame					jointVelAtObs;



	public TargetObjectStateEstimate(PositionEstimator pe, GazeDirectionComputer gdc,
				Rectangle targetRect, Long timestampMsec) {
		super(timestampMsec);
		jointPosAtObs  = pe.estimatePositionAtMoment(timestampMsec);
		jointVelAtObs = pe.estimateVelocityAtMoment(timestampMsec);

		myTargetDirectionAtObs = gdc.computeGazeDirection(jointPosAtObs, targetRect);
		myGazeSpeedAtObs = gdc.computeGazeVelocity(jointVelAtObs);
	}

	public double getPositionUncertainty() {
		double gazeVelAz = myGazeSpeedAtObs.getAzimuth().getDegrees();
		double gazeVelEl = myGazeSpeedAtObs.getElevation().getDegrees();
		double euclidNormSq = gazeVelAz * gazeVelAz + gazeVelEl * gazeVelEl;
		double euclidNorm = Math.sqrt(euclidNormSq);
		return euclidNorm;
	}
	public boolean isBetterThan(TargetObjectStateEstimate other, FaceNoticeConfig sightModelConfig) {
		if(sightModelConfig.ageUncertaintyWeight == null) {
			return false;
		}
		// Higher signed numbers mean "I am more certain than other"

		// Positive means I am newer than other
		double	ageDiffSec = (myTimeAtObs - other.myTimeAtObs) / 1000.0;
		// Positive means I am more pos-certain than other
		double  puDiff = other.getPositionUncertainty() - getPositionUncertainty();
		double  scoreDiff = sightModelConfig.ageUncertaintyWeight * ageDiffSec +
				sightModelConfig.positionUncertaintyWeight * puDiff;
		return (scoreDiff > 0.0);
	}
	public String toString() {
		return "TOSE[timestamp=" + myTimeAtObs + ", posUncertainty=" + getPositionUncertainty() + "]";
	}
}
