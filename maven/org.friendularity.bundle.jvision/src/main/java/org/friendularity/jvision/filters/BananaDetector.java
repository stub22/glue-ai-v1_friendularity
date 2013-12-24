package org.friendularity.jvision.filters;


import org.friendularity.jvision.gui.FileLocations;
import org.opencv.core.Scalar;
import org.opencv.objdetect.CascadeClassifier;
/*
 * Detect bananas
 * 
 */
public class BananaDetector extends CascadeDetector {
    
	@Override
	protected FileLocations.CascadeType cascadeType() {
		return FileLocations.CascadeType.BANANA_CASCADE;
	}

	@Override
	protected Scalar boxColor() {
		return new Scalar(0, 255, 255);
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
		return "banana detector"; 
	}
	
	
}
