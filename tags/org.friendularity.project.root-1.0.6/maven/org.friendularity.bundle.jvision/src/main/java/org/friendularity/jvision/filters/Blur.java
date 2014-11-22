package org.friendularity.jvision.filters;

import javax.swing.JFrame;
import org.opencv.core.Mat;
import org.opencv.core.Size;
import org.opencv.imgproc.Imgproc;
/*
 * blur with kernal size 5
 */
public class Blur extends ApplicativeBaseFilter implements ParamChangedListener {
	private Size kernal = new Size(13.0, 13.0);
	private double kernalSize = 13.0d;
	
	@Override
	public void apply(Mat in, Mat out) {
		Imgproc.blur(in, out, kernal);
	}
	
	@Override
	public String toString() {
		return "blur"; 
	}

	@Override
	public void showParamUI(JFrame parent) {
		FilterParams.showParamWidget(this, "blur radius,slider,1," + kernalSize + ",36");
	}

	@Override
	public String serialize() {
		return Double.toString(kernalSize);
	}

	@Override
	public void deserialize(String str) {
		kernalSize = Double.parseDouble(str);
		kernal = new Size(kernalSize, kernalSize);
	}

	@Override
	public void paramChanged(int index, String param) {
		kernalSize = Double.parseDouble(param);
		kernal = new Size(kernalSize, kernalSize);		
	}
}
