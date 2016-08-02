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
 * Created by Stub22 on 7/27/2016.
 *
 * Issues addressed here:
 * 1A) Thread-context classloader is important during launch.
 *
 * 1B) On first run the broker config comes from the "initial-config.json" file found
 * in root resources dir ("/") of that context classloader.  This location may be overriden by
 * System.property args, as discussed in QPid docs.  But we are not doing that presently.
 *
 * 1C) Our initial-config.json currently calls for 2 memory-based exchanges, named
 * "test" (compat with defaults from QPid v0.10-v0.26) and "friendu-vhn".
 *
 * The memory-based structure (rather than derby-based) is indicated by this arg encoded below:
 * qpid.initial_config_virtualhost_config={ "type" : "Memory" }
 *
 * 2) HOME dir is generally readonly, while WORK dir writability is important on first run,
 * and optionally for further hacking of QPid config between subsequent runs.
 *
 * 3) Qpid broker wants to configure Log4J as discussed here:
 * https://qpid.apache.org/releases/qpid-0.32/java-broker/book/Java-Broker-Runtime.html#Java-Broker-Runtime-Log-Files
 * At runtime this overwrites our existing Log4J config (set up by our boss bundle).  Boo!
 * So we want to counteract that, preferably by preventing it, or alternately
 * restoring our setup after broker launch, or telling QPid to use our log file instead, or...
 *
 * 4) On failure during launch, QPid broker will call   java system  .shutdown(), and halt
 * the Java VM.
 *
 */
public class QPidBrokerLauncher {

	static public String DEFAULT_HOME_DIR_PATH = "qpid_broker_home/";
	static public String DEFAULT_WORK_DIR_PATH = "qpid_broker_work/";

	public static void launchBrokerWithDfltArgs(BundleContext bunCtx) {
		launchBrokerWithDirpaths(bunCtx, DEFAULT_HOME_DIR_PATH, DEFAULT_WORK_DIR_PATH);
	}

	public static void launchBrokerWithDirpaths(BundleContext bunCtx,
				String homeDirPath, String workDirPath) {
		// This arg setup can be done instead through System.Properties or env vars, whatever.
		String[] pseudoArgs = {
				"-prop", "qpid.home_dir=" + homeDirPath,
				"-prop", "qpid.work_dir=" + workDirPath,
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
		// Our class derived from Main addresses issue #2 in comments at top, shutting off
		// QPid's configuration of Log4J.  Also addresses #4, by making process-shutdown optional
		// on failure.

		QPidBrokerMain.ourMainMethod(args);

		// Source of Main.java is here in the top qpid-broker project.
		// http://grepcode.com/file/repo1.maven.org/maven2/org.apache.qpid/qpid-broker/0.32/org/apache/qpid/server/Main.java?av=f

		// Source of Broker.java and BrokerOptions.java is in the qpid-broker-core project.
		// http://grepcode.com/file/repo1.maven.org/maven2/org.apache.qpid/qpid-broker-core/0.32/org/apache/qpid/server/Broker.java?av=f



	}


}
