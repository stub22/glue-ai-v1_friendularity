package org.friendularity.jvision.filters;

import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.core.MatOfRect;
import org.opencv.core.Point;
import org.opencv.core.Rect;
import org.opencv.core.Scalar;
import org.opencv.objdetect.CascadeClassifier;

import org.friendularity.jvision.gui.FileLocations;
/*
 * Detect facial profiles.
 * Only works going to the right (TBD - flip the image and run there)
 * Requires a slightly munged version of lbpcascade_profileface.xml
 * Delete the comment at the top of yours
 * 
 */
public class ProfileDetector implements BaseFilter {
    // Create a face detector from the cascade file in the resources
    // directory.
	private static CascadeClassifier faceDetector = new CascadeClassifier(FileLocations.lbpCascadeBase() + "lbpcascade_profileface.xml");
	@Override
	public void apply(Mat in, Mat out) {
	    // Detect faces in the image.
	    // MatOfRect is a special container class for Rect.
	    MatOfRect faceDetections = new MatOfRect();
	    faceDetector.detectMultiScale(in, faceDetections);

	    in.clone().copyTo(out);
	    
	    // Draw a bounding box around each face.
	    for (Rect rect : faceDetections.toArray()) {
	        Core.rectangle(out, new Point(rect.x, rect.y), new Point(rect.x + rect.width, rect.y + rect.height), new Scalar(255, 255, 0));
	    }
	}

}
