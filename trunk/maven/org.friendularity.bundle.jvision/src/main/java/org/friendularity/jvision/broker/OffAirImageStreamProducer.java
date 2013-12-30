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

import org.appdapter.core.log.BasicDebugger;

/**
 *
 * @author Annie
 */
public class OffAirImageStreamProducer extends BasicDebugger implements SwitchableImageStreamProducer {
	private SimpleImageStreamProducer isp = null;
	
	OffAirImageStreamProducer(String name) {
		isp = new SimpleImageStreamProducer(name);
		
		UrOffAirImageStreamProducer.getDefaultUrOffAirImageStreamProducer().addConsumer(isp);
	}

	void dispose() {
		/*
		 * In the case where a customer has already accessed the ImageStreamBroker to get one of these producers
		 * *before* JVisionLauncher has run, then when JVision launcher does run we were getting:
     [java] 65614  ERROR [FelixDispatchQueue] (MacroBundleActivatorBase.java:845) dispatchFrameworkStartedEvent0 - 
	 * handleFrameworkStartedEvent java.lang.UnsupportedOperationException: Not supported yet.
     [java] 	at org.friendularity.jvision.broker.OffAirImageStreamProducer.dispose(OffAirImageStreamProducer.java:32)
     [java] 	at org.friendularity.jvision.broker.ImageStreamBroker.addImageStreamProducer(ImageStreamBroker.java:67)
     [java] 	at org.friendularity.jvision.engine.JVisionEngine.<init>(JVisionEngine.java:64)
     [java] 	at org.friendularity.jvision.engine.JVisionEngine.getDefaultJVisionEngine(JVisionEngine.java:55)
     [java] 	at org.friendularity.jvision.gui.JVisionLauncher.<init>(JVisionLauncher.java:29)
     [java] 	at org.friendularity.bundle.jvision.JVisionBundleActivator.launchJVisionDemo(JVisionBundleActivator.java:62)
     [java] 	at org.friendularity.bundle.jvision.JVisionBundleActivator.handleFrameworkStartedEvent(JVisionBundleActivator.java:39)
	 * 
	 * ...which prevented JVision from completing its init.
	 *	So, Stu removed the exception and added a log message, instead.
	*/
		// throw new UnsupportedOperationException("Not supported yet.");
		getLogger().warn("OffAirStream is being disposed of, is that what we want?");
		/* But now we get
     [java] # A fatal error has been detected by the Java Runtime Environment:
     [java] #
     [java] #  EXCEPTION_ACCESS_VIOLATION (0xc0000005) at pc=0x000000006dc3a39b, pid=11312, tid=7260
	 * so apparently there is more to do here!
	 * Stu has gone back to using 
	 */
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
