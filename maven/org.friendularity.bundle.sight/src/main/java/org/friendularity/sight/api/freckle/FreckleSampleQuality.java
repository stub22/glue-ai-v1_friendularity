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

import java.io.Serializable;
import java.util.logging.Logger;

/**
 *
 * @author Stu B. <www.texpedient.com>
 */
public class FreckleSampleQuality implements Serializable {
	private static Logger theLogger = Logger.getLogger(FreckleSampleQuality.class.getName());

	private String	myRawQualityPacket;
	private Double	myScore = -888.0;
	private Boolean myEnrollmentSuitability = false;
	private Boolean myVerificationSuitability = false;
	private String	myQualityHints = "result parsing error";

	public FreckleSampleQuality(String sampleQualityPacket) {
		myRawQualityPacket = sampleQualityPacket;
		if ((sampleQualityPacket == null) || sampleQualityPacket.isEmpty()) {
			theLogger.warning("Sample quality packet is empty");
			return;
		}
		if (sampleQualityPacket.trim().equals("NO_EYES_FOUND")) {
			myQualityHints = "NO_EYES_FOUND";
			myScore = -555.0;
			theLogger.info("Limited quality info: " + sampleQualityPacket);
		} else {
			String sqTokens[] = sampleQualityPacket.split("\\/", -1);
			if (sqTokens.length == 4) {
				myScore = Double.parseDouble(sqTokens[0]);
				myEnrollmentSuitability = sqTokens[1].equals("1");
				myVerificationSuitability = sqTokens[2].equals("1");
				myQualityHints = sqTokens[3];
			} else {
				theLogger.warning("Got unexpected sampleQuality token count: " + sqTokens.length + " for tokenString: " + sampleQualityPacket);
			}
		}
	}
	public boolean checkEnrollmentWorthy() {
		return myEnrollmentSuitability && myVerificationSuitability	&& (myScore > 0.9);
	}

	public Double getScore() {
		return myScore;
	}

	public void setScore(Double score) {
		myScore = score;
	}

	public Boolean getVerificationSuitability() {
		return myVerificationSuitability;
	}

	public void setVerificationSuitability(Boolean vsuit) {
		myVerificationSuitability = vsuit;
	}

	public Boolean getEnrollmentSuitability() {
		return myEnrollmentSuitability;
	}

	public void setEnrollmentSuitability(Boolean enrollmentSuitability) {
		myEnrollmentSuitability = enrollmentSuitability;
	}
	public String getQualityHints() {
		return myQualityHints;
	}

	public void setQualityHints(String qualityHints) {
		myQualityHints = qualityHints;
	}
	public String getRawPacket() {
		return myRawQualityPacket;
	}
	public void setRawPacket(String raw) {
		myRawQualityPacket = raw;
	}
	public String toString() {
		return "score=" + myScore
				+ ", enrollSuit=" + myEnrollmentSuitability
				+ ", verifySuit=" + myVerificationSuitability
				+ ", qualityHints=[" + myQualityHints + "]"
				+ ", rawPacket=[" + myRawQualityPacket + "]";
	}
	public String fetchSummary() {
		return String.format("q=%+6.4f", getScore()) + ", vs=" + getVerificationSuitability()
				+ ", es=" + getEnrollmentSuitability() + ", hints=" + getQualityHints();
	}
}
