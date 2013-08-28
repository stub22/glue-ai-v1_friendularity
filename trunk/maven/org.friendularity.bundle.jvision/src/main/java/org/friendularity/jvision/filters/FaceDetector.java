package org.friendularity.jvision.filters;

import java.io.*;
import java.util.*;
import java.io.FileWriter;
import org.friendularity.jvision.gui.FileLocations;

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
public class FaceDetector extends CascadeDetector {
	protected FileLocations.CascadeType cascadeType() {
		return FileLocations.CascadeType.FRONTAL_CASCADE;
	}
}
