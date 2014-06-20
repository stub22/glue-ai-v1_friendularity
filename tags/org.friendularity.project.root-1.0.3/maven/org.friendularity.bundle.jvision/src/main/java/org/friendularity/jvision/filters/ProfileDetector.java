package org.friendularity.jvision.filters;


import org.friendularity.jvision.gui.FileLocations;
import org.opencv.core.Scalar;
import org.opencv.objdetect.CascadeClassifier;
/*
 * Detect facial profiles.
 * Only works going to the right (TBD - flip the image and run there)
 * Requires a slightly munged version of lbpcascade_profileface.xml
 * Delete the comment at the top of yours
 * 
 */
public class ProfileDetector extends CascadeDetector {
    
	@Override
	protected FileLocations.CascadeType cascadeType() {
		return FileLocations.CascadeType.PROFILE_CASCADE;
	}

	@Override
	protected Scalar boxColor() {
		return new Scalar(255, 0, 255);
	}
	
	
	// Create a face detector from the cascade file in the resources
	// directory.
	protected static CascadeClassifier detector = null;
	
	/**
	 * @return the detector
	 */
	protected CascadeClassifier getDetector() {
		return detector;
	}

	/**
	 * @param aDetector the detector to set
	 */
	protected void setDetector(CascadeClassifier aDetector) {
		detector = aDetector;
	}
	

	@Override
	public String toString() {
		return "profile_detector"; 
	}
}
