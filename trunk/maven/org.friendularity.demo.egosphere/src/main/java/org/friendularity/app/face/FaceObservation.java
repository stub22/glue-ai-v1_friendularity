/*
 *  Copyright 2008 Hanson Robotics Inc.
 *  All Rights Reserved.
 */

package org.friendularity.app.face;

import org.cogchar.api.freckler.protocol.FaceRecognitionStatus;
import org.cogchar.api.freckler.protocol.FreckleResult;
import org.cogchar.api.sight.SightObservation;
import org.cogchar.sight.vision.PortableImage;
import org.friendularity.app.freckle.FreckleFace;

import java.awt.Rectangle;

import java.util.logging.Logger;

import static org.cogchar.api.freckler.protocol.FaceRecognitionStatus.*;
/**
 * @author Stu Baurmann
 */
public class FaceObservation extends SightObservation {
	private static Logger theLogger = Logger.getLogger(FaceObservation.class.getName());
	
	// public  long					frameNumber;
	private	Object					servoSnapshot;

	private PortableImage			myPortableFaceImage, 
									myPortableShirtSampleImage;

	private FreckleFace				myFreckleFace;
	private	Double					myFreckleMatchStrength;
	private String					myFreckleSampleQualitySummary;
	private FreckleResult			myFreckleResult;
	private FaceRecognitionStatus	myRecognitionStatus = UNTRIED;

	private	Long					myFriendID = null;

	private Long					myFreckbaseID = null;

	public FaceObservation() {
	}
	public FaceRecognitionStatus getRecognitionStatus() {
		return myRecognitionStatus;
	}
	public void setRecognitionStatus(FaceRecognitionStatus recstat) {
		myRecognitionStatus = recstat;
	}	
	public void setFaceImage(PortableImage pi) {
		myPortableFaceImage = pi;
	}
	public PortableImage getFaceImage() {
		return myPortableFaceImage;
	}
	public void setShirtSampleImage(PortableImage pi) {
		myPortableShirtSampleImage = pi;
	}
	public PortableImage getShirtSampleImage() {
		return myPortableShirtSampleImage;
	}
	public FreckleFace getFreckleFace() {
		return myFreckleFace;
	}
	public void setFreckleFace(FreckleFace ff) {
		myFreckleFace = ff;
	}
	public Double getFreckleMatchStrength() {
		return myFreckleMatchStrength;
	}
	public void setFreckleMatchStrength(Double fms) {
		myFreckleMatchStrength = fms;
	}
	public void verifyFreckleMatchAttemptEligibility() {
		if (getRecognitionStatus() != UNTRIED) {
			throw new RuntimeException("Attempted to freckle-match observation with status: " + myRecognitionStatus);
		}
		if (myFreckleFace != null) {
			throw new RuntimeException("Attempted to freckle-match observation which already has f-face: " + myFreckleFace);
		}
	}

	public Rectangle getShirtSampleRect(Rectangle fieldBoundsRect) {
		// Return a "good" rectangle for shirt sampling, based on the face bound rect,
		// without exceeding the boundary of the fieldBoundsRect.
		// This rectangle should be in the same coordinate system as the face bound rect,
		// which is the one reported by the native code, and the one used to draw our screen
		// anotations (but NOT the same as what we use to actually grab pixels).
		// If the face is too close to the bottom of the field, there may not be a viable
		// shirt sample rect, in which case this method should return null.
		Rectangle faceRect = getBoundRect();
		double faceCenterX = faceRect.getCenterX();
		double faceCenterY = faceRect.getCenterY();
		double faceWidth = faceRect.getWidth();
		double faceHeight = faceRect.getHeight();

		double shirtCenterX = faceCenterX;
		double shirtCenterY = faceCenterY + 1.7 * faceHeight;
		double shirtHeight = faceHeight / 2;
		double shirtWidth = faceWidth;

		int shirtUpperLeftX = (int) (shirtCenterX - shirtWidth / 2);
		int shirtUpperLeftY = (int) (shirtCenterY - shirtHeight / 2);

		Rectangle naiveShirtRect = new Rectangle(shirtUpperLeftX, shirtUpperLeftY, (int) shirtWidth, (int) shirtHeight);
		theLogger.finer("Naive Shirt Rect=" + naiveShirtRect);
		Rectangle croppedShirtRect = null;
		if (naiveShirtRect.intersects(fieldBoundsRect)) {
			croppedShirtRect = naiveShirtRect.intersection(fieldBoundsRect);
			theLogger.finer("Cropped Shirt Rect=" + croppedShirtRect);
		}
		return croppedShirtRect;
	}

	public Object getServoSnapshot() {
		return servoSnapshot;
	}

	public void setServoSnapshot(Object servoSnapshot) {
		this.servoSnapshot = servoSnapshot;
	}

	public String getFreckleSampleQualitySummary() {
		return myFreckleSampleQualitySummary;
	}
	public void setFreckleSampleQualitySummary(String summary) {
		myFreckleSampleQualitySummary = summary;
	}
	public void setFreckleResult (FreckleResult fres) {
		myFreckleResult = fres;
	}
	public FreckleResult getFreckleResult () {
		return myFreckleResult;
	}
	public Long getFriendID() {
		return myFriendID;
	}
	public String getFriendPermCueID() {
		Long friendID = getFriendID();
		return (friendID != null) ? ("fbf_" + friendID) : null;
	}
	public void syncFromFreckbaseObs(org.cogchar.freckbase.Observation fbo) {
		String recogStatus = fbo.myRecogStatus();
		// get(default) is deprecrated, BUT
		// getOrElse requires a function argument
		String qualityPacket = fbo.myQualityPacket().getOrElse(null);
		Long friendID = fbo.myFriendID().getOrElse(null);
		myFriendID = friendID;
	}
	
}
