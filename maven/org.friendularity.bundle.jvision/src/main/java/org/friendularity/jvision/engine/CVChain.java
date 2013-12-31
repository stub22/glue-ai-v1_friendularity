 /*
 *  Copyright 2013 by The Friendularity Project (www.friendularity.org).
 * 
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 * 
 *       http://www.apache.org/licenses/LICENSE-2.0
 * 
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
package org.friendularity.jvision.engine;

import java.awt.image.BufferedImage;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.ListModel;
import javax.swing.SwingWorker;
import javax.swing.text.Document;
import org.friendularity.jvision.broker.ImageFlavorNotAvailable;
import org.friendularity.jvision.broker.ImageStreamBroker;
import org.friendularity.jvision.broker.ImageStreamConsumer;
import org.friendularity.jvision.broker.ImageStreamImage;
import org.friendularity.jvision.broker.ImageStreamProducer;
import org.friendularity.jvision.broker.SimpleImageStreamProducer;
import org.friendularity.jvision.filters.FilterSequence;
import org.opencv.core.Mat;

/**
 *
 * @author Annie
 */
public class CVChain implements ImageStreamConsumer {
	protected String chainName;
	protected boolean intermediatesVisible;
	protected String source;
	protected FilterSequence filters;
	protected SimpleImageStreamProducer outputProducer;
	
	/**
	 * The only thing that should call this is CVChainManager
	 * @param chainName
	 * @param intermediatesVisible
	 * @param source 
	 */
	CVChain(String chainName, boolean intermediatesVisible, String source) {
		this.chainName = chainName;
		this.intermediatesVisible = intermediatesVisible;
		this.source = source;
		filters = new FilterSequence();

		init();
	}
	
	private void init() {
		Logger.getLogger(CVChain.class.getName()).log(Level.INFO, "about to wire up imagestream");
		ImageStreamBroker.getDefaultImageStreamBroker().alwaysAddImageStreamConsumer(source, this);
		outputProducer = new SimpleImageStreamProducer(getOutName());
		ImageStreamBroker.getDefaultImageStreamBroker().addImageStreamProducer(outputProducer);
		Logger.getLogger(CVChain.class.getName()).log(Level.INFO, "done wiring up imagestream");
	}

	public String getName() {
		return chainName;
	}

	public void setPublishIntermediates(boolean publish) {
		// TODO need to publish or unpublish
		intermediatesVisible = publish;
	}

	public String getSource() {
		return source;
	}

	public String getOutName() {
		return chainName + ".out";
	}

	/**
	 * This is a tad iffy since it's letting a mutable structure out of the bag
	 * 
	 * @return 
	 */
	public ListModel getFilterSequence() {
		return filters;
	}

	// =============== ImageStreamConsumer ==========================
	@Override
	public void setConsumedImage(ImageStreamImage in) {
		Mat out = new Mat();
		try {
			filters.apply(in.getMat(), out);
			outputProducer.setConsumedImage(new ImageStreamImage(out));
		} catch (ImageFlavorNotAvailable ex) {
			Logger.getLogger(CVChain.class.getName()).log(Level.SEVERE, null, ex);
		}
	}

	@Override
	public void setConsumedMessage(String string) {
		
	}

	@Override
	public void sourceIsEnding() {
		outputProducer.sourceIsEnding();
		ImageStreamBroker.getDefaultImageStreamBroker().removeImageStreamConsumerAllStreams(this);
		ImageStreamBroker.getDefaultImageStreamBroker().removeImageStreamProducer(outputProducer.getSourceName());
	}

}
