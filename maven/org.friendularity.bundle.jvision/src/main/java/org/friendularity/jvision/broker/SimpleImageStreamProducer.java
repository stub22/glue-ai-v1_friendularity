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
package org.friendularity.jvision.broker;

import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.Iterator;

/**
 * An ImageStreamProducer that is also a consumer. You can instantiate one of these and
 * feed the consumer interface images as a simple way of being an image producer
 * 
 * @author Annie
 */
public class SimpleImageStreamProducer implements ImageStreamConsumer, SwitchableImageStreamProducer {

	private ArrayList<ImageStreamConsumer>consumers = new ArrayList<ImageStreamConsumer>();

	private String name;
	
	public SimpleImageStreamProducer(String name) {
		this.name = name;
	}
	
	@Override
	public void addConsumer(ImageStreamConsumer c) {
		synchronized(consumers)
		{
			consumers.add(c);
		}
	}

	@Override
	public void removeAllConsumers() {
		synchronized(consumers)
		{
			for(Iterator<ImageStreamConsumer>i = consumers.iterator() ; i.hasNext() ; )
			{
				ImageStreamConsumer isc = i.next();

				isc.sourceIsEnding();
			}

			consumers = new ArrayList<ImageStreamConsumer>();
		}
	}

	@Override
	public String getSourceName() {
		return name;
	}

	@Override
	public void setConsumedImage(ImageStreamImage img) {
		synchronized(consumers)
		{
			for(Iterator<ImageStreamConsumer>i = consumers.iterator() ; i.hasNext() ; )
			{
				ImageStreamConsumer isc = i.next();

				isc.setConsumedImage(img);
			}
		}
	}

	@Override
	public void setConsumedMessage(String string) {
		synchronized(consumers)
		{
			for(Iterator<ImageStreamConsumer>i = consumers.iterator() ; i.hasNext() ; )
			{
				ImageStreamConsumer isc = i.next();

				isc.setConsumedMessage(string);
			}
		}
	}

	@Override
	public void sourceIsEnding() {
		this.removeAllConsumers();
	}
	
	public boolean hasConsumers() {
		return consumers.size() > 0;
	}

	@Override
	public void switchTo(ImageStreamProducer p) {
		for(Iterator<ImageStreamConsumer>i = consumers.iterator() ; i.hasNext() ; )
		{
			ImageStreamConsumer isc = i.next();

			p.addConsumer(isc);
		}
		
		removeAllConsumers();
	}

	@Override
	public void removeConsumer(ImageStreamConsumer c) {
		consumers.remove(c);
	}
			
}
