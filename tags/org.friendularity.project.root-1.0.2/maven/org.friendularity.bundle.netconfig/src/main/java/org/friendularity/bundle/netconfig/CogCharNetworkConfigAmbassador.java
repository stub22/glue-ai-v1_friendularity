/*
 *  Copyright 2012 by The Cogchar Project (www.cogchar.org).
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
package org.friendularity.bundle.netconfig;

import org.cogchar.bind.lift.LiftAmbassador;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author Ryan Biggs <rbiggs@skyriversoftware.com>
 */
public class CogCharNetworkConfigAmbassador implements LiftAmbassador.LiftNetworkConfigInterface {
	
	private static Logger theLogger = Logger.getLogger(CogCharNetworkConfigAmbassador.class.getName());
	
	@Override
	public void configure(String ssid, String security, String key) {
		try {
			theLogger.log(Level.INFO, "Configuring network with SSID: {0} and security type: {1}", new Object[]{ssid, security});
			WiFiSecurity securityType = WiFiSecurity.valueOf(security);
			NetworkConfigurator configurator = new LinuxNetworkConfigurator();
			configurator.configureNetwork(new NetworkConfig(ssid, securityType, key));
		} catch (Exception e) {
			theLogger.log(Level.WARNING, "Could not match provided input \"{0}\" with a valid security type", security);
		}
	}
	
}
