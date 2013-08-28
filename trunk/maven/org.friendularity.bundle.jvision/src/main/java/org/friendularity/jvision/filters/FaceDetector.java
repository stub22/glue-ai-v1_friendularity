package org.friendularity.jvision.filters;

import java.io.*;
import java.util.*;
import java.io.FileWriter;

import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.core.MatOfRect;
import org.opencv.core.Point;
import org.opencv.core.Rect;
import org.opencv.core.Scalar;
import org.opencv.objdetect.CascadeClassifier;

/*
 * blur with kernal size 5
 */
public class FaceDetector implements BaseFilter {
	// Create a face detector from the cascade file in the resources
	// directory.
	private static CascadeClassifier faceDetector;
	static {
		InputStream inputStream = null;
		BufferedReader br = null;

		try {
			File outputs = File.createTempFile("File", ".lbpcascade_frontalface.xml");
			FileWriter fw = new FileWriter(outputs);

			// read this file into InputStream
			inputStream = CascadeClassifier.class.getResourceAsStream("/opencv/lbpcascade/lbpcascade_frontalface.xml");

			br = new BufferedReader(new InputStreamReader(inputStream));

			StringBuilder sb = new StringBuilder();

			String line;
			while ((line = br.readLine()) != null) {
				fw.write(line + "\n");
			}
		
			System.out.println("\nDone!");

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
			if (br != null) {
				try {
					br.close();
				} catch (IOException e) {
					e.printStackTrace();
				}
			}
		}
	}

	@Override public void apply(Mat in, Mat out) {
		// Detect faces in the image.
		// MatOfRect is a special container class for Rect.
		MatOfRect faceDetections = new MatOfRect();
		faceDetector.detectMultiScale(in, faceDetections);

		in.clone().copyTo(out);

		// Draw a bounding box around each face.
		for (Rect rect : faceDetections.toArray()) {
			Core.rectangle(out, new Point(rect.x, rect.y), new Point(rect.x + rect.width, rect.y + rect.height), new Scalar(0, 255, 0));
		}
	}

}
