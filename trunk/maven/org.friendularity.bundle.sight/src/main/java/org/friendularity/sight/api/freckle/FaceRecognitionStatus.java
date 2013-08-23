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
public enum FaceRecognitionStatus {
	UNTRIED,		// Recognition process has not started yet
	PROCESSING,		// Recognition in progress
	MATCH_ACCEPTED,		// Recognition succeeded
	ENROLLED,		// New FIR record enrolled 
	FAILED,			// Not matched (or match not accepted), not enrolled
	ERROR;			// Probably failed, but also, something is just WRONG

	public boolean attemptComplete() {
		return !((this == UNTRIED) || (this == PROCESSING));
	}
	public boolean matchedOrEnrolled() {
		return (this == MATCH_ACCEPTED) || (this == ENROLLED);
	}
}
