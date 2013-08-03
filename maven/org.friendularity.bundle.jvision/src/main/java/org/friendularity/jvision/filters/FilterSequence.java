package org.friendularity.jvision.filters;

import java.util.ArrayList;

import org.opencv.core.Mat;

/* 
 * An ordered sequence of filters to be applied
 */
public class FilterSequence implements BaseFilter {
	
	private ArrayList<BaseFilter> filters = new ArrayList<BaseFilter>();

	@Override
	public void apply(Mat in, Mat out) {
		Mat temp = new Mat();
		
		in.copyTo(temp);
		applyIndexed(temp, 0, out);
		
	}
	
	private void applyIndexed(Mat in, int index , Mat out){
		if(index >= filters.size()) {
			in.copyTo(out);
		} else {
			BaseFilter f = filters.get(index);
			Mat temp = new Mat();
			f.apply(in, temp);
			applyIndexed(temp, index + 1 , out);
		}
	}
	
	public void addOrReplaceByClass(BaseFilter f) {
		for(int i = filters.size() - 1 ; i >= 0 ; i--){

			if(f.getClass().isInstance(filters.get(i))) {
				filters.set(i, f);
				return;
			}
		}
		filters.add(f);
	}
	
	public void removeByClass(BaseFilter f) {
		for(int i = filters.size() - 1 ; i >= 0 ; i--){

			if(f.getClass().isInstance(filters.get(i))) {
				filters.remove(i);
			}
		}		
	}

}
