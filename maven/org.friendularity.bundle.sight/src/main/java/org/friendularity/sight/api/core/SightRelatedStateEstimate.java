/*
 *  Copyright 2013 by The Friendularity Project (www.friendularity.org).
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
package org.friendularity.sight.api.core;

import java.awt.Rectangle;
import org.cogchar.animoid.calc.estimate.PositionEstimator;
import org.cogchar.api.animoid.protocol.EgocentricDirection;
import org.friendularity.sight.api.freckle.FaceNoticeConfig;

/**
 *
 * @author Stu B22 <stub22@appstract.com>
 */
public class SightRelatedStateEstimate {
	public Long						myTimeAtObs;
	public EgocentricDirection		myTargetDirectionAtObs;
	public EgocentricDirection		myGazeSpeedAtObs;
	
	// This is here so we can display on GUI
	public SightObservation			myTempHackedSightObservation;
	
	public SightRelatedStateEstimate(Long timestampMsec) { 
		myTimeAtObs = timestampMsec;
		
	}
	public boolean isBetterThan(SightRelatedStateEstimate other, FaceNoticeConfig sightModelConfig) {
		return false;
	}
}
