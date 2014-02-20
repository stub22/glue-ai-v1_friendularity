package org.friendularity.jvision.filters;

import javax.swing.JFrame;
import org.opencv.core.Mat;
import org.opencv.imgproc.Imgproc;

public class Grayscale extends ApplicativeBaseFilter {

	@Override
	public void apply(Mat in, Mat out) {
		Imgproc.cvtColor(in, out, Imgproc.COLOR_RGB2GRAY);
	}


	@Override
	public String toString() {
		return "gray"; 
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
