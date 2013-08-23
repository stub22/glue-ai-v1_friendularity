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

package org.friendularity.sight.api.core;

import java.awt.Rectangle;
import java.io.Serializable;
// import org.cogchar.animoid.calc.estimate.TargetObjectStateEstimate;
import org.cogchar.api.animoid.protocol.EgocentricDirection;
import org.cogchar.platform.util.TimeUtils;

/**
 * @author Stu B. <www.texpedient.com>
 */
public class SightObservation implements Serializable {
	private	long						myTimeStampMsec;
	private	EgocentricDirection			myCenterDirection;   // computed from boundRect + servoSnapshot + kinematics
	// public	TargetObjectStateEstimate	myTOSE;
	public	SightRelatedStateEstimate		myTOSE;
	private	Rectangle					myBoundRect;
	
	
	public EgocentricDirection getCenterDirection() {
		return myCenterDirection;
	}

	public long getTimeStampMsec() {
		return myTimeStampMsec;
	}

	public void setCenterDirection(EgocentricDirection centerDirection) {
		myCenterDirection = centerDirection;
	}

	public void setTimeStampMsec(long timeStampMsec) {
		myTimeStampMsec = timeStampMsec;
	}
	
	public Rectangle getBoundRect() {
		return myBoundRect;
	}

	public void setBoundRect(Rectangle boundRect) {
		myBoundRect = boundRect;
	}
	public Double getPixelArea() {
		return myBoundRect.getWidth() * myBoundRect.getHeight();
	}

	public Double getDiameterPixels() {
		// Pretend the enclosed area is a circle.  Yeah, that's the ticket!
		double area = getPixelArea();
		double radius = Math.sqrt(area/Math.PI);
		return 2.0 * radius;
	}
	public double getDiameterDeg(SightPort vp) {
		return getDiameterPixels() * vp.getGeometricMeanDegPerPixel();
	}
	public double getAgeSec() {
		return TimeUtils.getStampAgeSec(myTimeStampMsec);
	}

}
