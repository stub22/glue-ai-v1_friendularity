package org.friendularity.jvision.filters;

import org.opencv.core.Mat;

/*
 * Abstract base class for an image processing filter
 * 
 */
public interface BaseFilter {
	/*
	 * apply this filter
	 */
	public void apply(Mat in, Mat out);

}
