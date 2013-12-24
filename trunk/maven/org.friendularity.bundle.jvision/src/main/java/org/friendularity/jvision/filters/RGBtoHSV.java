package org.friendularity.jvision.filters;

import org.opencv.core.Mat;
import org.opencv.imgproc.Imgproc;

public class RGBtoHSV implements BaseFilter {

	@Override
	public void apply(Mat in, Mat out) {
		Imgproc.cvtColor(in, out, Imgproc.COLOR_BGR2HSV);
	}
	

	@Override
	public String toString() {
		return "rgb2hsv"; 
	}
}
