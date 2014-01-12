package org.friendularity.jvision.filters;

import javax.swing.JFrame;
import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.imgproc.Imgproc;
import org.opencv.core.Scalar;




public class ColorThreshold implements BaseFilter, ParamChangedListener {

	public static final int MIN_H = 0;
	public static final int MIN_S = 1;
	public static final int MIN_V = 2;
	public static final int MAX_H = 3;
	public static final int MAX_S = 4;
	public static final int MAX_V = 5;
	
	private float[] params = {0.0f, 0.0f, 0.0f, 255.0f, 255.0f, 255.0f};
	
	@Override
	public void apply(Mat in, Mat out) {
    Mat hsv = new Mat();
	Imgproc.cvtColor(in, hsv, Imgproc.COLOR_BGR2HSV);

    Core.inRange(hsv, new Scalar(
			params[ColorThreshold.MIN_H], 
			params[ColorThreshold.MIN_S], 
			params[ColorThreshold.MIN_V]), 
		new Scalar(
			params[ColorThreshold.MAX_H], 
			params[ColorThreshold.MAX_S], 
			params[ColorThreshold.MAX_V]), 
		out);
	}
	
	@Override
	public String toString() {
		return "color_threshold"; 
	}

	@Override
	public void showParamUI(JFrame parent) {
		FilterParams.showParamWidget(this, "min H or B,slider,0," + params[0] + ",256," +
				"min S or G,slider,0," + params[1] + ",256," +
				"min V or R,slider,0," + params[2] + ",256," +
				"max H or B,slider,0," + params[3] + ",256," +
				"max S or G,slider,0," + params[4] + ",256," +
				"max V or R,slider,0," + params[5] + ",256"
				);
	}

	@Override
	public String serialize() {
		return "" + params[0] + "," + params[1] + "," +
				params[2] + "," + params[3] + "," +
				params[4] + "," + params[5];
	}

	@Override
	public void deserialize(String str) {
		String[] s = str.split(",");
		
		for(int i = 0 ; i < 6 ; i++ ) {
			params[i] = Float.parseFloat(s[i]);
		}
	}

	@Override
	public void paramChanged(int index, String param) {
		params[index] = Float.parseFloat(param);
	}
}
