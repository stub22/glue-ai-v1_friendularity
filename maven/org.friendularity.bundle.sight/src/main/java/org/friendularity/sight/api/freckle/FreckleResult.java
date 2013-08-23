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
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

import org.cogchar.platform.util.StringUtils;

/**
 * FreckleResult must be both serializable (for JMX notifications) and MXBean-composite-data
 * mappable (to be returned as result of syncQuery).
 * @author Stu B. <www.texpedient.com>
 */
public class FreckleResult implements Serializable {
	private static Logger theLogger = Logger.getLogger(FreckleResult.class.getName());

	public static class PossibleMatch implements Serializable {
		private	String		myFreckleID;
		private	Double		myMatchStrength;
		public PossibleMatch() {
		}
		public PossibleMatch(String matchDataPair) {
			String matchDataTokens[] = matchDataPair.split(":");
			myFreckleID = matchDataTokens[0];
			myMatchStrength = Double.parseDouble(matchDataTokens[1]);
		}
		public String toString() {
			return "Alternative[freckleID=" + myFreckleID
				+ ", strength=" + myMatchStrength
				+ "]";
		}
		public String getFreckleID() {
			return myFreckleID;
		}
		public Double getMatchStrength() {
			return myMatchStrength;
		}
		public void setFreckleID(String fid) {
			myFreckleID = fid;
		}
		public void setMatchStrength(Double strength) {
			myMatchStrength = strength;
		}
	}

	public enum EnrollmentAction {
		NO_ATTEMPT_BECAUSE_PREVENT_THRESH_MISSING_OR_NEG,
		NO_ATTEMPT_BECAUSE_ALREADY_MATCHED,
		NO_ATTEMPT_BECAUSE_POOR_QUALITY,
		ATTEMPT_FAILED,
		SUCCESS;
		public boolean enrollAttempted() {
			return ((this == ATTEMPT_FAILED) || (this == SUCCESS));
		}
	}

	private	String					mySubmittedHandle;
	// This extra population info is bundled to reduce network chat and sync penalties.
	private	String					myPopulationFreckleIDs[];
	private List<PossibleMatch>		myAlternativeMatches;
	private	String					myEnrolledFreckleID;
	private	FreckleSampleQuality	mySampleQuality;
	private EnrollmentAction		myEnrollmentAction;
	public FreckleResult() {
	}
	public void appendAlternativeForMatchDataPair(String matchDataPair) {
		if (matchDataPair.contains(":")) {
			PossibleMatch a = new PossibleMatch(matchDataPair);
			myAlternativeMatches.add(a);
		} else {
			theLogger.warning("No matchDataPair found in string: " + matchDataPair);
		}
	}
	public void parseMatchingResult(String matchResult) {
		if ((matchResult != null) && (!matchResult.isEmpty())) {
			
			String resultPacketPieces[] = matchResult.split("\\$", -1);
			
			if (resultPacketPieces.length > 0) {
				theLogger.info("First piece of match string=[" + resultPacketPieces[0] + "]");
				String matchAlternativesPacket = resultPacketPieces[0];
				if (matchAlternativesPacket.length() > 0) {
					String matchBlocks[] =  resultPacketPieces[0].split(",");
					myAlternativeMatches = new ArrayList<PossibleMatch>(); // [matchBlocks.length];
					for (int i=0; i < matchBlocks.length; i++) {
						appendAlternativeForMatchDataPair(matchBlocks[i]);
					}
				}
			} 	else {
				theLogger.severe("Unable to parse match+quality result string: " + matchResult);
			}
			String sampleQualityPacket = null;
			if (resultPacketPieces.length == 2) {
				theLogger.info("Second half of match string=[" + resultPacketPieces[1] + "]");
				sampleQualityPacket = resultPacketPieces[1];
			}
			mySampleQuality = new FreckleSampleQuality(sampleQualityPacket);
				
		}  else {
			theLogger.severe("Got empty match+quality result string");
		}
	}

	public String toString() {
		return "FreckleResult["
				+ "handle=" + mySubmittedHandle
				+ ", sampleQuality=[" + mySampleQuality +"]"
				+ ", matchAlts=" + myAlternativeMatches
				+ ", enrollAct=" + myEnrollmentAction
				+ ", enrolledFreckleID=" + myEnrolledFreckleID
				+ ", popFreckleIDs=" + 
				((myPopulationFreckleIDs != null)?StringUtils.joinArray(myPopulationFreckleIDs, ", "):null)
				+ "]";
	}
	public PossibleMatch fetchBestAlternative() {
		if ((myAlternativeMatches != null) && (myAlternativeMatches.size() > 0)) {
			return myAlternativeMatches.get(0);
		} else {
			return null;
		}
	}
	public String fetchSampleQualitySummary() {
		if (mySampleQuality != null) {
			return mySampleQuality.fetchSummary();
		} else {
			return "[ERROR - NO QUALITY SUMMARY]";
		}
	}
	public FreckleSampleQuality fetchSampleQuality() {
		return mySampleQuality;
	}
	public boolean checkEnrollmentWorthy(Double matchScoreEnrollPreventThresh) {
		if ((matchScoreEnrollPreventThresh == null) || (matchScoreEnrollPreventThresh < 0.0)) {
			theLogger.info("Not enrolling new freckle-face because matchScoreEnrollPreventThresh=" + matchScoreEnrollPreventThresh);
			setEnrollmentAction(EnrollmentAction.NO_ATTEMPT_BECAUSE_PREVENT_THRESH_MISSING_OR_NEG);
			return false;
		}
		PossibleMatch bestAlt = fetchBestAlternative();
		if (bestAlt != null) {
			if (bestAlt.myMatchStrength >= matchScoreEnrollPreventThresh) {
				theLogger.info("Not enrolling new freckle-face because best match strength  " 
						+ bestAlt.myMatchStrength + " is above threshold " + matchScoreEnrollPreventThresh);
				setEnrollmentAction(EnrollmentAction.NO_ATTEMPT_BECAUSE_ALREADY_MATCHED);
				return false;
			} else {
				theLogger.info("Consider enrolling new freckle-face because best match score "
						+ bestAlt.myMatchStrength + " is below threshold " + matchScoreEnrollPreventThresh);
			}
		} else {
			theLogger.info("Consider enrolling new freckle-face because no potential matches exist.");
		}
		if (mySampleQuality.checkEnrollmentWorthy()) {
			theLogger.info("Sample quality is OK for enrollment: " + mySampleQuality);
			return true;
		} else {
			theLogger.info("Sample quality is too low for enrollment: " + mySampleQuality);
			setEnrollmentAction(EnrollmentAction.NO_ATTEMPT_BECAUSE_POOR_QUALITY);
			return false;
		}
	}

	public List<PossibleMatch> getAlternatives() {
		return myAlternativeMatches;
	}

	public void setAlternatives(List<PossibleMatch> alternatives) {
		myAlternativeMatches = alternatives;
	}

	public EnrollmentAction getEnrollmentAction() {
		return myEnrollmentAction;
	}
	public void setEnrollmentAction(EnrollmentAction ea) {
		myEnrollmentAction = ea;
	}

	public String getEnrolledFreckleID() {
		return myEnrolledFreckleID;
	}

	public void setEnrolledFreckleID(String enrolledFreckleID) {
		myEnrolledFreckleID = enrolledFreckleID;
	}


	public String getSubmittedHandle() {
		return mySubmittedHandle;
	}

	public void setSubmittedHandle(String submittedHandle) {
		mySubmittedHandle = submittedHandle;
	}


	public String[] getPopulationFreckleIDs() {
		return myPopulationFreckleIDs;
	}

	public void setPopulationFreckleIDs(String[] popFreckleIDs) {
		myPopulationFreckleIDs = popFreckleIDs;
	}


}
