package org.friendularity.jvision.filters;

import org.friendularity.jvision.gui.HSVSliders;
import org.opencv.core.Core;
import org.opencv.core.Mat;
import org.opencv.imgproc.Imgproc;
import org.opencv.video.Video;
import org.opencv.core.CvType;
import org.opencv.core.Point;
import org.opencv.core.Scalar;




public class ColorThreshold implements BaseFilter {

	public static final int MIN_H = 0;
	public static final int MIN_S = 1;
	public static final int MIN_V = 2;
	public static final int MAX_H = 3;
	public static final int MAX_S = 4;
	public static final int MAX_V = 5;
	
	private static float[] params = {50.0f, 50.0f, 50.0f, 205.0f, 205.0f, 205.0f};
	
	public static void showHSVSliders() {
		HSVSliders dlg = new HSVSliders();
		dlg.setVisible(true);
	}

	public static void changeLimit(int value, int which) {
		params[which]  = (float)value;
	}
	
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
	
/*
    // first frame
    if(mLastFrame == null)
    {
      mLastFrame = new Mat();
      Imgproc.cvtColor(in, mLastFrame, Imgproc.COLOR_RGB2GRAY);
      in.copyTo(out);
      return;
    }
    
    Video.calcOpticalFlowFarneback(mLastFrame, hsv, mFlowImage, 0.5, 3, 15, 3, 5, 1.2, 0);
    
    in.copyTo(out);
    drawOptFlowMap(mFlowImage, out);
    hsv.copyTo(mLastFrame);
	*/
	}
}
