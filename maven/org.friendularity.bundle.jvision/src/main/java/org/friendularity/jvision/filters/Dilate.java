package org.friendularity.jvision.filters;

import org.opencv.core.Mat;
import org.opencv.core.Size;
import org.opencv.imgproc.Imgproc;
/*
 * blur with kernal size 5
 */
public class Dilate implements BaseFilter {
	private static Size kernal_size = new Size(13, 13);
	@Override
	public void apply(Mat in, Mat out) {
		Mat kernal = Imgproc.getStructuringElement(Imgproc.MORPH_ELLIPSE, kernal_size);
		
		Imgproc.dilate(in, out, kernal);
	}

	
	@Override
	public String toString() {
		return "dilate_13"; 
	}
}
