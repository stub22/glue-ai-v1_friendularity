package org.friendularity.jvision.filters;

import javax.swing.JFrame;
import org.opencv.core.Mat;
import org.opencv.core.Size;
import org.opencv.imgproc.Imgproc;
/*
 * blur with kernal size 5
 */
public class Dilate implements BaseFilter, ParamChangedListener {
	private Size kernal_size = new Size(13, 13);
	private int dilateIterations = 13;
	
	@Override
	public void apply(Mat in, Mat out) {
		Mat kernal = Imgproc.getStructuringElement(Imgproc.MORPH_ELLIPSE, kernal_size);
		
		Imgproc.dilate(in, out, kernal);
	}

	
	@Override
	public String toString() {
		return "dilate"; 
	}

	@Override
	public void showParamUI(JFrame parent) {
		FilterParams.showParamWidget(this, "dilation iterations,slider,0," + dilateIterations + ",24");
	}

	@Override
	public String serialize() {
		return Integer.toString(dilateIterations);
	}

	@Override
	public void deserialize(String str) {
		dilateIterations = (int)Double.parseDouble(str);
		kernal_size = new Size(dilateIterations, dilateIterations);
	}

	@Override
	public void paramChanged(int index, String param) {
		dilateIterations = (int)Double.parseDouble(param);
		kernal_size = new Size(dilateIterations, dilateIterations);		
	}
}
