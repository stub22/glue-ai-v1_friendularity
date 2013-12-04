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
 * @author Annie
 */
public class OffAirImageStreamProducer implements SwitchableImageStreamProducer {
	private SimpleImageStreamProducer isp = null;
	
	OffAirImageStreamProducer(String name) {
		isp = new SimpleImageStreamProducer(name);
		
		UrOffAirImageStreamProducer.getDefaultUrOffAirImageStreamProducer().addConsumer(isp);
	}

	void dispose() {
		throw new UnsupportedOperationException("Not supported yet.");
	}

	@Override
	public String getSourceName() {
		return isp.getSourceName();
	}

	@Override
	public void addConsumer(ImageStreamConsumer c) {
		isp.addConsumer(c);
	}

	@Override
	public void removeAllConsumers() {
		isp.removeAllConsumers();
	}

	@Override
	public void switchTo(ImageStreamProducer p) {
		isp.switchTo(p);
	}
}
