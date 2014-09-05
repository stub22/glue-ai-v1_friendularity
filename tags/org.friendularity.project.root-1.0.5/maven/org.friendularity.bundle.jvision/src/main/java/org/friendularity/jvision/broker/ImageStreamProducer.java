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

/**
 *
 * An ImageStreamProducer can register itself as a producer.
 * 
 * Most users will find it easier to instantiate a SimpleImageStreamProducer and delegate the work to it.
 * 
 * @author Annie
 */
public interface ImageStreamProducer  {
	
	/**
	 *  The name of this image stream. ImageStreamBroker will enforce system wide uniqueness on these names
	 * by refusing to register existing names. 
	 * 
	 * Names currently have a dot structure from general to specific. So jvision.out is the output tap from jvision,
	 * jvision.camera is the raw camera feed, and so on
	 * At some point we may migrate towards an rdf URI scheme
	 * 
	 * @return the name of this image stream. 
	 * 
	 */
	public String getSourceName();

	/**
	 * Add a consumer to this imagestream
	 * 
	 * @param c 
	 */
	public void addConsumer(ImageStreamConsumer c);

	/**
	 * Remove all consumers from the producer. Typically this is called just
	 * before the Producer ceases. Implementers are required to conform to the
	 * contract of calling sourceEnding() on all consumers prior to this
	 * 
	 */
	public void removeAllConsumers();

	/**
	 * remove a single consumer. ISP implementers must call sourceIsEnding on the consumer before removing it.
	 * 
	 * @param c the consumer to remove
	 */
	public void removeConsumer(ImageStreamConsumer c);
}
