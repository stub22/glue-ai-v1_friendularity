package org.friendularity.jvision.filters;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import org.friendularity.jvision.gui.FileLocations;
import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.core.MatOfRect;
import org.opencv.core.Point;
import org.opencv.core.Rect;
import org.opencv.core.Scalar;
import org.opencv.objdetect.CascadeClassifier;

/**
 *  Superclass for all detectors that use CascadeClassifier
 * 
 * @author Annie
 */


public abstract class CascadeDetector implements BaseFilter {
	
	protected abstract FileLocations.CascadeType cascadeType();
	
	// Create a face detector from the cascade file in the resources
	// directory.
	protected static CascadeClassifier faceDetector = null;
	
	protected void ensureTransient() {
		if (faceDetector != null)
			return;
		InputStream inputStream = null;
		BufferedReader br = null;
		FileWriter fw = null;
		File outputs = null;
		try {
			outputs = File.createTempFile("File", 
					FileLocations.cascadeTempfileName(cascadeType()));
			fw = new FileWriter(outputs);

			// read this file into InputStream
			inputStream = CascadeClassifier.class.getResourceAsStream(
					FileLocations.cascadeResourceLocation(cascadeType()));

			br = new BufferedReader(new InputStreamReader(inputStream));

			String line;
			while ((line = br.readLine()) != null) {
				fw.write(line + "\n");
			}
			try {
				fw.close();
			} catch (IOException e) {
				e.printStackTrace();
			}
			
			faceDetector = new CascadeClassifier(outputs.getAbsolutePath());

		} catch (IOException e) {
			e.printStackTrace();
		} finally {
			if (inputStream != null) {
				try {
					inputStream.close();
				} catch (IOException e) {
					e.printStackTrace();
				}
			}

		}
	}
	
	public void apply(Mat in, Mat out) {
		try {
			ensureTransient();
			// Detect faces in the image.
			// MatOfRect is a special container class for Rect.
			MatOfRect faceDetections = new MatOfRect();
			faceDetector.detectMultiScale(in, faceDetections);

			in.clone().copyTo(out);

			// Draw a bounding box around each face.
			for (Rect rect : faceDetections.toArray()) {
				Core.rectangle(out, new Point(rect.x, rect.y), new Point(rect.x + rect.width, rect.y + rect.height), new Scalar(0, 255, 0));
			}
		} catch (Throwable t) {
			t.printStackTrace();
			in.clone().copyTo(out);
		}
	}
}
