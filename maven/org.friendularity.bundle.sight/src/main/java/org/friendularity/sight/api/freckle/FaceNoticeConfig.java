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

package org.friendularity.sight.api.freckle;

/**
 *
 * @author Stu B. <www.texpedient.com>
 */
public class FaceNoticeConfig {
	public Double		initialStrength;
	
	// 2 exponential decay constants.  HIDDEN = face is known to be outside FOV, EXPOSED = in FOV
	public Double		hiddenDecayConstant;
	public Double		exposedDecayConstant;
	
	public Double		postingThreshold;
	public Double		clearingThreshold;
	public Double		survivalThreshold;

	public Integer		obsRetainedInHypo;
	public Integer		obsRetainedInFreckleFace;
	
	
	public Double		cogDistCoeffAzDiamSquared;
	public Double		cogDistCoeffElDiamSquared;
	public Double		cogDistCoeffSeconds;
	public Double		cogDistCoeffProduct;

	public Double		cogDistCoeffTimestampOverlap;
	public Double		cogDistCoeffFreckleMatch;
	public Double		cogDistCoeffFreckleMismatch;

	public Double		mergeThresholdCogDist;
/*
 *		Correction factor for using vision timestamp to estimate position.
 *		How much slower is the camera+openCV stack than the serial-to-servo stack?
 *		Use a negative number if we think serial-to-servo is slower.
 */
	public Double		visionToMotionOffsetSec;

	public Double		ageUncertaintyWeight;
	public Double		positionUncertaintyWeight;


	
	public String toString() {
		return "FaceNoticeConfig[" +
				"\ninitialStrength=" + initialStrength +
				"\nhiddenDecayConstant=" + hiddenDecayConstant +
				"\nexposedDecayConstant=" + exposedDecayConstant +
				"\npostingThreshold=" + postingThreshold +
				"\nclearingThreshold=" + clearingThreshold +
				"\nsurvivalThreshold=" + survivalThreshold +
				"\nobsRetainedInHypo=" + obsRetainedInHypo +
				"\nobsRetainedInFreckleFace=" + obsRetainedInFreckleFace +
				"\ncogDistCoeffAzDiamSquared=" + cogDistCoeffAzDiamSquared +
				"\ncogDistCoeffElDiamSquared=" + cogDistCoeffElDiamSquared +
				"\ncogDistCoeffSeconds=" + cogDistCoeffSeconds +
				"\ncogDistCoeffTimestampOverlap=" + cogDistCoeffTimestampOverlap +
				"\ncogDistCoeffFreckleMatch=" + cogDistCoeffFreckleMatch +
				"\ncogDistCoeffFreckleMismatch=" + cogDistCoeffFreckleMismatch +	
				// "\ncogDistCoeffProduct=" + cogDistCoeffProduct +
				"\nmergeThresholdCogDist=" + mergeThresholdCogDist +
				"\nvisionToMotionOffsetSec=" + visionToMotionOffsetSec +
				"\nageUncertaintyWeight=" + ageUncertaintyWeight +
				"\npositionUncertaintyWeight=" + positionUncertaintyWeight +
				"]";
	}
}
