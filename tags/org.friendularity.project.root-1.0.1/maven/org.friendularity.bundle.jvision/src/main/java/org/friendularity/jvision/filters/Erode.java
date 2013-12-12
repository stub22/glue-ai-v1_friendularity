package org.friendularity.jvision.filters;

import org.opencv.core.Mat;
import org.opencv.core.Size;
import org.opencv.imgproc.Imgproc;
/*
 * blur with kernal size 5
 */
public class Erode implements BaseFilter {
	private static Size kernal_size = new Size(13, 13);
	@Override
	public void apply(Mat in, Mat out) {
		Mat kernal = Imgproc.getStructuringElement(Imgproc.MORPH_ELLIPSE, kernal_size);
		
		Imgproc.erode(in, out, kernal);
	}

}
