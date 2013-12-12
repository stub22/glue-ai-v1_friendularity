package org.friendularity.jvision.filters;

import java.util.ArrayList;
import java.util.HashMap;
import org.appdapter.core.log.BasicDebugger;
import org.friendularity.jvision.broker.ImageStreamBroker;
import org.friendularity.jvision.broker.ImageStreamProducer;
import org.friendularity.jvision.broker.SimpleImageStreamProducer;
import org.friendularity.jvision.engine.JVisionEngine;

import org.opencv.core.Mat;

/* 
 * An ordered sequence of filters to be applied
 */
public class FilterSequence extends BasicDebugger implements BaseFilter {
	
	private ArrayList<BaseFilter> filters = new ArrayList<BaseFilter>();
	
	private HashMap<String, SimpleImageStreamProducer>broadcasters = new HashMap<String, SimpleImageStreamProducer>();

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
			broadcast(temp, f.getClass().getSimpleName());
			
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

	private void broadcast(Mat temp, String msg) {
		String fname = "jvision.filter." + msg;
		
		if(!broadcasters.containsKey(fname))
		{
			SimpleImageStreamProducer sisp = 
					new SimpleImageStreamProducer(fname);
			ImageStreamBroker.getDefaultImageStreamBroker().addImageStreamProducer(sisp);
			broadcasters.put(fname, sisp);
		}
		SimpleImageStreamProducer isp = broadcasters.get(fname);
		
		if(isp.hasConsumers())
		{
			try
			{
				isp.setConsumedImage(JVisionEngine.matToBufferedImage(temp));
				isp.setConsumedMessage(msg);
			} catch(IllegalArgumentException e) {
				getLogger().warn("Could not create BufferedImage (usually height/width zero)");
			}
		}		
		
	}

}
