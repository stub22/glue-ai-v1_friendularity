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

/**
 *
 * @author Stu B. <www.texpedient.com>
 */
public class FreckleMatchConfig implements Serializable {
	// IF all existing freckle-faces match with a score less than this threshold,
	// AND the enrollment image is of sufficiently good quality, then it can be
	// enrolled as a new face.  If null, then enrollment is disabled.
	private	Double			matchScorePreventEnrollThresh;

	private	Double			matchScoreAcceptThresh;
	private	Double			matchScoreExpandThresh;
	private	Integer			maxProfileWidth;

	public Double getMatchScoreAcceptThresh() {
		return matchScoreAcceptThresh;
	}

	public void setMatchScoreAcceptThresh(Double thresh) {
		this.matchScoreAcceptThresh = thresh;
	}

	public Double getMatchScoreExpandThresh() {
		return matchScoreExpandThresh;
	}

	public void setMatchScoreExpandThresh(Double thresh) {
		this.matchScoreExpandThresh = thresh;
	}

	public Double getMatchScorePreventEnrollThresh() {
		return matchScorePreventEnrollThresh;
	}

	public void setMatchScorePreventEnrollThresh(Double thresh) {
		this.matchScorePreventEnrollThresh = thresh;
	}

	public Integer getMaxProfileWidth() {
		return maxProfileWidth;
	}

	public void setMaxProfileWidth(Integer width) {
		this.maxProfileWidth = width;
	}


	@Override public String toString() {
		return "FreckleMatchConfig["
			+ "\npreventEnrollThresh=" + matchScorePreventEnrollThresh
			+ "\nmatchScoreAcceptThresh=" + matchScoreAcceptThresh
			+ "\nmatchScoreExpandThresh=" + matchScoreExpandThresh
			+ "\nmaxProfileWidth=" + maxProfileWidth
			+ "]";
	}
}
