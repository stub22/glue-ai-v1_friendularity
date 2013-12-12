package org.friendularity.jvision.filters;

import org.opencv.core.Mat;
import org.opencv.core.Size;
import org.opencv.imgproc.Imgproc;
/*
 * blur with kernal size 5
 */
public class Blur implements BaseFilter {
	private static Size kernal = new Size(13, 13);
	@Override
	public void apply(Mat in, Mat out) {
		Imgproc.blur(in, out, kernal);
	}

}
