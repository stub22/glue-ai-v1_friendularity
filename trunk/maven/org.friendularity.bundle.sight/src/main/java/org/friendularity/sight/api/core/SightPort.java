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


import java.awt.Point;

import org.cogchar.api.animoid.protocol.SmallAngle;


/**
 *
 * @author Stu B. <www.texpedient.com>
 */
public class SightPort {
	private		Integer			widthPixels, heightPixels;
	private		Double			widthDegrees, heightDegrees;
	private		Double			azSkewDegrees, elSkewDegrees;
	private		SmallAngle		myWidthAngle, myHeightAngle;
	private		SmallAngle		myAzSkewAngle, myElSkewAngle;

	private	static SmallAngle theZeroAngle = SmallAngle.makeFromDeg(0.0);
	public Integer getWidthPixels() {
		return widthPixels;
	}

	public Integer getHeightPixels() {
		return heightPixels;
	}

	public SmallAngle getWidthAngle() {
		if (myWidthAngle == null) {
			myWidthAngle = SmallAngle.makeFromDeg(widthDegrees);
		}
		return myWidthAngle;
	}
	public SmallAngle getHeightAngle() {
		if (myHeightAngle == null) {
			myHeightAngle = SmallAngle.makeFromDeg(heightDegrees);
		}
		return myHeightAngle;
	}
	public SmallAngle getAzSkewAngle() {
		if (myAzSkewAngle == null) {
			myAzSkewAngle = SmallAngle.makeFromDeg(azSkewDegrees);
		}
		return myAzSkewAngle;
	}
	public SmallAngle getElSkewAngle() {
		if (myElSkewAngle == null) {
			myElSkewAngle = SmallAngle.makeFromDeg(elSkewDegrees);
		}
		return myElSkewAngle;
	}
	public boolean inBounds(Point p) {
		return ((p.x >= 0) && (p.y >= 0) && (p.x < widthPixels) && (p.y < heightPixels));
	}
	public SmallAngle getAzimuthAngleForScreenHorizPixel(int pixel) {
		SmallAngle naiveAngle = getAngleForPixel(pixel,  getWidthPixels(),  getWidthAngle(), false);
		SmallAngle skewAngle = getAzSkewAngle();
		return naiveAngle.add(skewAngle);
	}
	public SmallAngle getElevationAngleForScreenVertPixel(int pixel) {
		SmallAngle naiveAngle = getAngleForPixel(pixel,  getHeightPixels(),  getHeightAngle(), true);
		SmallAngle skewAngle = getElSkewAngle();
		return naiveAngle.add(skewAngle);
	}
	public int getCameraHorizPixelForAzimuthAngle(SmallAngle naiveAngle) {
		SmallAngle skewAngle = getAzSkewAngle();
		SmallAngle trueAngle = naiveAngle.subtract(skewAngle);
		return getPixelForAngle(trueAngle, getWidthPixels(),  getWidthAngle(), false);
	}	
	public int getCameraVertPixelForElevationAngle(SmallAngle naiveAngle) {
		SmallAngle skewAngle = getElSkewAngle();
		SmallAngle trueAngle = naiveAngle.subtract(skewAngle);
		return getPixelForAngle(trueAngle, getHeightPixels(),  getHeightAngle(), true);
	}		
	public Point getCameraCenterPixel() {
		int centerX = getCameraHorizPixelForAzimuthAngle(theZeroAngle);
		int centerY = getCameraVertPixelForElevationAngle(theZeroAngle);
		return new Point(centerX, centerY);
	}
	public SmallAngle getCameraWidthToRightOfCenter() {
		SmallAngle skewAngle = getAzSkewAngle();
		SmallAngle fullWidthAngle = getWidthAngle();
		SmallAngle halfWidthAngle = fullWidthAngle.multiply(0.5);
		return halfWidthAngle.add(skewAngle);
	}
	public SmallAngle getCameraWidthToLeftOfCenter() {
		SmallAngle skewAngle = getAzSkewAngle();
		SmallAngle fullWidthAngle = getWidthAngle();
		SmallAngle halfWidthAngle = fullWidthAngle.multiply(0.5);
		return halfWidthAngle.subtract(skewAngle);
	}
	public SmallAngle getCameraHeightAboveCenter() {
		SmallAngle skewAngle = getElSkewAngle();
		SmallAngle fullHeightAngle = getHeightAngle();
		SmallAngle halfHeightAngle = fullHeightAngle.multiply(0.5);
		return halfHeightAngle.add(skewAngle);
	}
	public SmallAngle getCameraHeightBelowCenter() {
		SmallAngle skewAngle = getElSkewAngle();
		SmallAngle fullHeightAngle = getHeightAngle();
		SmallAngle halfHeightAngle = fullHeightAngle.multiply(0.5);
		return halfHeightAngle.subtract(skewAngle);
	}
	public SmallAngle getMinViewableAzForCenter(SmallAngle centerAz) {
		return centerAz.subtract(getCameraWidthToLeftOfCenter());
	}
	public SmallAngle getMaxViewableAzForCenter(SmallAngle centerAz) {
		return centerAz.add(getCameraWidthToRightOfCenter());
	}
	public SmallAngle getMinViewableElForCenter(SmallAngle centerAz) {
		return centerAz.subtract(getCameraHeightBelowCenter());
	}
	public SmallAngle getMaxViewableElForCenter(SmallAngle centerAz) {
		return centerAz.add(getCameraHeightAboveCenter());
	}

	private SmallAngle getAngleForPixel(int pixel, int fullScreenPixels, SmallAngle fullScreenAngle,
			boolean inverted) {
		double centerPixel = fullScreenPixels / 2.0;
		double pixelDisplace = (double) pixel - centerPixel;
		double tanHSA = getHalfScreenAngleTangent(fullScreenAngle);
		double ratio = pixelDisplace * 2.0 * tanHSA / fullScreenPixels;
		double angleRad = Math.atan(ratio);
		if (inverted) {
			angleRad = -1.0 * angleRad;
		}
		return SmallAngle.makeFromRad(angleRad);	
	}		
	private int getPixelForAngle(SmallAngle angle, int fullScreenPixels, SmallAngle fullScreenAngle,
			boolean inverted) {

		double tanHSA = getHalfScreenAngleTangent(fullScreenAngle);
		
		double angleRad = angle.getRadians();
		if (inverted) {
			angleRad = -1.0 * angleRad;
		}
		double ratio = Math.tan(angleRad);
		double pixelDisplace = ratio * fullScreenPixels / ( 2.0 * tanHSA);
		double centerPixel = fullScreenPixels / 2.0;
		int pixel = (int) Math.round(centerPixel + pixelDisplace);
		return pixel;
	}		
	
	private double getHalfScreenAngleTangent (SmallAngle fullScreenAngle) {
		double fullScreenAngleRad = fullScreenAngle.getRadians();
		double halfScreenAngleRad = fullScreenAngleRad / 2.0;
		double tanHSA = Math.tan(halfScreenAngleRad);		
		return tanHSA;
	}
	public double getMeanWidthDegPerPixel() { 
		return widthDegrees / widthPixels;
	}
	public double getMeanHeightDegPerPixel() {
		return heightDegrees / heightPixels;
	}
	public double getGeometricMeanDegPerPixel() {
		return Math.sqrt(getMeanWidthDegPerPixel() * getMeanHeightDegPerPixel());
	}
	public String toString() { 
		return "ViewPort[width pix=" + widthPixels + " angle=" + getWidthAngle()
					+ ", height pix=" + heightPixels + " angle=" + getHeightAngle()
					+ ", azSkew=" + getAzSkewAngle() + ", elSkew=" + getElSkewAngle()
					+ "]";
	}
	
}
