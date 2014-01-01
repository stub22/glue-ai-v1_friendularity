package org.friendularity.jvision.filters;

import org.opencv.core.Mat;

/*
 * All image processing filters implement BaseFilter
 * 
 */
public interface BaseFilter {
	/*
	 * apply this filter
	 */
	public void apply(Mat in, Mat out);

}
