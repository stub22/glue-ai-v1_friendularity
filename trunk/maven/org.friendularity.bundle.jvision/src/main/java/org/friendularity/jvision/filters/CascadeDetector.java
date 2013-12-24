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
	
	/**
	 * @return the detector
	 */
	protected abstract CascadeClassifier getDetector();

	/**
	 * @param aDetector the detector to set
	 */
	protected abstract void setDetector(CascadeClassifier aDetector);
	
	protected abstract FileLocations.CascadeType cascadeType();
	
	/*
	 * The classifier insists on reading from a file.
	 * We're getting info from a resource bundle, so we have to write a temp file and
	 * read it
	 * 
	 */
	protected void ensureTransient() {
		if (getDetector() != null)
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
			
			setDetector(new CascadeClassifier(outputs.getAbsolutePath()));

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
			getDetector().detectMultiScale(in, faceDetections);

			in.clone().copyTo(out);

			Scalar color = this.boxColor();
			// Draw a bounding box around each face.
			for (Rect rect : faceDetections.toArray()) {
				Core.rectangle(out, new Point(rect.x, rect.y), new Point(rect.x + rect.width, rect.y + rect.height), color);
			}
		} catch (Throwable t) {
			t.printStackTrace();
			in.clone().copyTo(out);
		}
	}

	protected abstract Scalar boxColor();
	
	
	@Override
	public String toString() {
		return "abstract_cascade_detector"; 
	}
}
