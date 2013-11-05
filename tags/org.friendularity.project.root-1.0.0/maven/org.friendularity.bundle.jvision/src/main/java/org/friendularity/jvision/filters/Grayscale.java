package org.friendularity.jvision.filters;

import org.opencv.core.Mat;
import org.opencv.imgproc.Imgproc;

public class Grayscale implements BaseFilter {

	@Override
	public void apply(Mat in, Mat out) {
		Imgproc.cvtColor(in, out, Imgproc.COLOR_RGB2GRAY);
	}

}
