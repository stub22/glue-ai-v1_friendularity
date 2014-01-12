package org.friendularity.jvision.filters;

import javax.swing.JFrame;
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
	
	public void showParamUI(JFrame parent);
	
	public String serialize();
	
	public void deserialize(String str);

}
