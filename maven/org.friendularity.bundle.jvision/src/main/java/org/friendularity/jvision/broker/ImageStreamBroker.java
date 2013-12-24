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
 * ImageStreamBroker
 * 
 * A singleton class. The ImageStreamBroker allows systems that produce 'video' (a stream of images) to register
 * via a name (at some point this will be refactored to a URI). 
 * 
 * Image consumers can then register as ImageStreamConsumers via the name. 
 * 
 * Typical ImageStreamProducers would be JVision's filters, the physical robot's eyes, various virtual cameras
 * in ccrk, or various points in the image processing chain. 
 * 
 * At system startup, image consumers and producers may come online in any order. To avoid complex logic dealing
 * with the short period while the system is 'warming up', it is possible to register as a consumer of an 'always on'
 * producer. If the producer doesn't exist, the consumer will be attached to a default fallback producer that shows
 * an animated filmstrip leader. When the real producer appears, it takes over from the default.
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
			{
				ImageStreamProducer previsp = imageStreams.get(isp.getSourceName());
				
				if (previsp instanceof OffAirImageStreamProducer)
				{
					OffAirImageStreamProducer offair = (OffAirImageStreamProducer) previsp;
					offair.switchTo(isp);
					offair.dispose();
				}
				else
					throw new IllegalArgumentException("Image Stream " + isp.getSourceName() + " already exists");
			}

			imageStreams.put(isp.getSourceName(), isp);
			imageStreams.notifyAll();
		}
	}
	
	public void removeImageStreamProducer(String name)
	{
		synchronized(imageStreams)
		{
			if(imageStreams.get(name) instanceof SwitchableImageStreamProducer)
			{
				SwitchableImageStreamProducer sisp = (SwitchableImageStreamProducer) imageStreams.get(name);
				OffAirImageStreamProducer offair = new OffAirImageStreamProducer(name);
				
				sisp.switchTo(offair);
				imageStreams.put(name, offair);
			}
			else
			{
				imageStreams.get(name).removeAllConsumers();
				imageStreams.remove(name);
			}
			imageStreams.notifyAll();
		}
	}
	
	/**
	 * Add the consumer as a listener to image stream 'name'
	 * creates a matching OffAirImageStreamProducer if the stream doesn't exist,
	 * so this always succeeds
	 * 
	 * @param name
	 * @param isc
	 */
	public void alwaysAddImageStreamConsumer(String name, ImageStreamConsumer isc)
	{
		synchronized(imageStreams)
		{
			ImageStreamProducer isp = imageStreams.get(name);
			
			if (isp == null)
			{
				isp = new OffAirImageStreamProducer(name);
				imageStreams.put(name, isp);
			}

			isp.addConsumer(isc);
		}		
	}
	
	/**
	 * Add the consumer as a listener to image stream 'name'
	 * Throws nullpointerexception if name does not exist
	 * 
	 * Deprecated  - consider alwaysAddImageStreamConsumer instead
	 * 
	 * @param name
	 * @param isc
	 * @deprecated
	 */
	@Deprecated
	public void addImageStreamConsumer(String name, ImageStreamConsumer isc)
	{
		synchronized(imageStreams)
		{
			ImageStreamProducer isp = imageStreams.get(name);

			isp.addConsumer(isc);
		}
	}
	
	/**
	 * Add the consumer as a listener to image stream 'name'
	 * Waits until the producer is available
	 * 
	 * Deprecated  - consider alwaysAddImageStreamConsumer instead
	 * 
	 * @param name
	 * @param isc
	 * @deprecated
	 */
	@Deprecated
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

	/**
	 * return true iff this is a currently unused prefix
	 * @param text
	 * @return 
	 */
	public boolean imageStreamPrefixOK(String text) {
		String asPrefix = text + ".";
		
		if(text.trim().length() < 1)return false;
		
		for(Iterator<String>names = imageStreamNames() ; names.hasNext() ; ) {
			String s = names.next();
			
			if(s.startsWith(asPrefix))return false;
			if(s.equals(text)) return false;
		}
		
		return true;
	}
}
