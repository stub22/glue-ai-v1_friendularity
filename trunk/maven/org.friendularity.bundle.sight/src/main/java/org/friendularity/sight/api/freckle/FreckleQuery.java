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

import org.friendularity.sight.vision.PortableImage;
import java.io.Serializable;

public class FreckleQuery implements Serializable {

	private String			myHandle;
	private PortableImage	myPortableImage;

	// THEN a new freckle-face should be created with this freckleID.
	private	String					myEnrollmentFreckleID;

	private Long					myFreckbaseObsID;

	private FreckleMatchConfig		myFreckleMatchConfig;

	public FreckleQuery() {
		
	}
	public FreckleQuery(String h, PortableImage pi, String enrollmentFID,
				FreckleMatchConfig conf) {
		myHandle = h;
		myEnrollmentFreckleID = enrollmentFID;
		myFreckleMatchConfig = conf;
		myPortableImage = pi;
	}
	public PortableImage getPortableImage() {
		return myPortableImage;
	}
	public void setPortableImage(PortableImage pimg) {
		myPortableImage = pimg;
	}
	public String getHandle() {
		return myHandle;
	}
	public void setHandle(String h) {
		myHandle = h;
	}

	public String getEnrollmentFreckleID() {
		return myEnrollmentFreckleID;
	}
	public void setEnrollmentFreckleID(String efid) {
		myEnrollmentFreckleID = efid;
	}
	public Long getFreckbaseObsID() {
		return myFreckbaseObsID;
	}
	public void setFreckbaseObsID(Long foid) {
		myFreckbaseObsID = foid;
	}
	public FreckleMatchConfig getFreckleMatchConfig() {
		return myFreckleMatchConfig;
	}
	public void setFreckleMatchConfig(FreckleMatchConfig fmc) {
		myFreckleMatchConfig = fmc;
	}
	public void disableAutoEnrollment() {
		myFreckleMatchConfig.setMatchScorePreventEnrollThresh(null);
	}	
	public String toString() {
		return "FreckleQuery[handle=" + myHandle 
			+ ", freckleMatchConf=" + myFreckleMatchConfig
			+ ", enrollFID=" + myEnrollmentFreckleID + "]";
	}
}
