package org.friendularity.jvision.filters;

import javax.swing.JFrame;
import org.opencv.core.Mat;
import org.opencv.core.Size;
import org.opencv.imgproc.Imgproc;
/*
 * blur with kernal size 5
 */
public class Erode implements BaseFilter, ParamChangedListener {
	private Size kernal_size = new Size(13, 13);
	private int erodeIterations = 13;
	
	@Override
	public void apply(Mat in, Mat out) {
		Mat kernal = Imgproc.getStructuringElement(Imgproc.MORPH_ELLIPSE, kernal_size);
		
		Imgproc.erode(in, out, kernal);
	}

	@Override
	public String toString() {
		return "erode"; 
	}
	

	@Override
	public void showParamUI(JFrame parent) {
		FilterParams.showParamWidget(this, "erosion iterations,slider,1," + erodeIterations + ",24");
	}

	@Override
	public String serialize() {
		return Integer.toString(erodeIterations);
	}

	@Override
	public void deserialize(String str) {
		erodeIterations = Integer.parseInt(str);
		kernal_size = new Size(erodeIterations, erodeIterations);
	}

	@Override
	public void paramChanged(int index, String param) {
		erodeIterations = Integer.parseInt(param);
		if(erodeIterations < 1)
			erodeIterations = 1;
		kernal_size = new Size(erodeIterations, erodeIterations);		
	}
}
