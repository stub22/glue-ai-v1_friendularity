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

import java.util.HashMap;
import java.util.Iterator;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author Annie
 */
public class ImageStreamBroker {
	private static ImageStreamBroker defBroker = null;
	
	private HashMap<String, ImageStreamProducer>imageStreams = new HashMap<String, ImageStreamProducer>();
	
	public static ImageStreamBroker getDefaultImageStreamBroker()
	{
		if(defBroker == null)
			defBroker = new ImageStreamBroker();
		
		return defBroker;
	}
	
	public void addImageStreamProducer(ImageStreamProducer isp)
	{
		synchronized(imageStreams)
		{
			if(imageStreams.containsKey(isp.getSourceName()))
				throw new IllegalArgumentException("Image Stream " + isp.getSourceName() + " already exists");

			imageStreams.put(isp.getSourceName(), isp);
			imageStreams.notifyAll();
		}
	}
	
	public void removeImageStreamProducer(String name)
	{
		synchronized(imageStreams)
		{
			imageStreams.get(name).removeAllConsumers();
			imageStreams.remove(name);
			imageStreams.notifyAll();
		}
	}
	
	public void addImageStreamConsumer(String name, ImageStreamConsumer isc)
	{
		synchronized(imageStreams)
		{
			ImageStreamProducer isp = imageStreams.get(name);

			isp.addConsumer(isc);
		}
	}
	
	public void waitAndAddImageStreamConsumer(String name, ImageStreamConsumer isc)
	{
		ImageStreamProducer isp;
		
		synchronized(imageStreams)
		{
			isp = imageStreams.get(name);

			while(isp == null)
			{
				try {
					Logger.getLogger(ImageStreamBroker.class.getName()).log(Level.INFO, "Waiting for stream " + name);
					imageStreams.wait(1000L);
				} catch (InterruptedException ex) {
					Logger.getLogger(ImageStreamBroker.class.getName()).log(Level.SEVERE, null, ex);
				}
				isp = imageStreams.get(name);
			}
		}
		isp.addConsumer(isc);
	}

	public Iterator<String> imageStreamNames() {
		return imageStreams.keySet().iterator();
	}
}
