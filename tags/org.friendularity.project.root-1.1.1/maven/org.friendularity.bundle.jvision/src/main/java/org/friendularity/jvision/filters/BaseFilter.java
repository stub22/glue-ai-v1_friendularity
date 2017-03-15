package org.friendularity.jvision.filters;

import javax.swing.JFrame;
import org.appdapter.core.log.BasicDebugger;
import org.friendularity.jvision.broker.ImageStreamConsumer;
import org.friendularity.jvision.broker.ImageStreamImage;
import org.friendularity.jvision.broker.ImageStreamProducer;
import org.friendularity.jvision.broker.SimpleImageStreamProducer;
import org.opencv.core.Mat;

/*
 * All image processing filters implement BaseFilter
 * 
 */
public abstract class BaseFilter extends BasicDebugger implements ImageStreamProducer, ImageStreamConsumer {
	protected SimpleImageStreamProducer isp;

	public BaseFilter() {
		super();
				
	   isp = new SimpleImageStreamProducer(this.getSourceName());
	}
	
	public abstract void showParamUI(JFrame parent);
	
	public abstract String serialize();
	
	public abstract void deserialize(String str);

	@Override
	public void addConsumer(ImageStreamConsumer c) {
		isp.addConsumer(c);
	}

	@Override
	public void removeAllConsumers() {
		isp.removeAllConsumers();
	}

	@Override
	public void removeConsumer(ImageStreamConsumer c) {
		isp.removeConsumer(c);
	}

	@Override
	public void setConsumedMessage(String string) {
		isp.setConsumedMessage(string);
	}

	@Override
	public void sourceIsEnding() {
		
	}

}
