package org.friendularity.jvision.filters;

import javax.swing.JFrame;
import org.opencv.core.Mat;
import org.opencv.imgproc.Imgproc;

public class RGBtoHSV extends ApplicativeBaseFilter {

	@Override
	public void apply(Mat in, Mat out) {
		Imgproc.cvtColor(in, out, Imgproc.COLOR_BGR2HSV);
	}
	

	@Override
	public String toString() {
		return "rgb2hsv"; 
	}

	@Override
	public void showParamUI(JFrame parent) {
		
	}

	@Override
	public String serialize() {
		return "";
	}

	@Override
	public void deserialize(String str) {
		
	}
}
