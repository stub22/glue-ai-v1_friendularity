/*
 *  Copyright 2016 by The Friendularity Project (www.friendularity.org).
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

package org.friendularity.bundle.qpid_broker_wrap;

import org.osgi.framework.BundleContext;

/**
 * Created by Owner on 7/27/2016.
 */
public class QPidBrokerLauncher {

	public static void launchBrokerWithDfltArgs(BundleContext context) {
		String[] pseudoArgs = {
				"-prop", "qpid.home_dir=qpid_broker_home/",
				"-prop", "qpid.work_dir=qpid_broker_work/",
				"-prop", "qpid.initial_config_virtualhost_config={ \"type\" : \"Memory\" }"

		};
		ClassLoader savedCL = Thread.currentThread().getContextClassLoader();
		ClassLoader bundleCL = org.apache.qpid.server.Broker.class.getClassLoader();
		try {
			if (bundleCL != null) {
				System.out.println("\n====\nSaved old classloader=" + savedCL + "\nSetting contextClassLoader to: " + bundleCL);
				Thread.currentThread().setContextClassLoader(bundleCL);
			}
			for (int i = 0; i< pseudoArgs.length ; i++) {
				System.out.println("Arg[" + i + "]=" + pseudoArgs[i]);
			}
			System.out.println("Launching broker main, which may catch exceptions and call shutdown(), which stops our log output!");
			launchBrokerMain(pseudoArgs);
			System.out.println("\n==========\nFinished launching broker main");
		} catch (Throwable th) {
			System.out.println(".start() caught exception: " + th);
			th.printStackTrace();
		} finally {
			System.out.println("\n==========\nSetting contextClassLoader back to: " + savedCL);
			Thread.currentThread().setContextClassLoader(savedCL);
		}
		System.out.println("\n==========\n.start() is returning");

	}
	static public void launchBrokerMain(String[] args) throws Throwable {
		// One big issue is that if this method catches a startup exception, it calls:
		//  shutdown(1);
		org.apache.qpid.server.Main.main(args);
	}


}
