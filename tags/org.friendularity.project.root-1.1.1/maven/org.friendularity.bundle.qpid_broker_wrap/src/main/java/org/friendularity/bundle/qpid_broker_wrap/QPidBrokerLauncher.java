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

import org.apache.qpid.server.BrokerOptions;
import org.apache.qpid.server.model.VirtualHostNode;
import org.appdapter.core.log.BasicDebugger;
import org.osgi.framework.BundleContext;
import org.slf4j.Logger;

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
 *
 * We have disabled this behavior.
 *
 * 4) On failure during launch, QPid broker will normally call   java system  .shutdown(), and halt
 * the Java VM.
 *
 * We have made this behavior optional.
 *
 *
 * ----
 [java] 5466    INFO [FelixDispatchQueue] org.friendularity.bundle.qpid_broker_wrap.QPidBrokerLauncher (QPidBrokerLauncher.java:104) launchBrokerWithDirpaths - Arg[0]=-prop
 [java] 5467    INFO [FelixDispatchQueue] org.friendularity.bundle.qpid_broker_wrap.QPidBrokerLauncher (QPidBrokerLauncher.java:104) launchBrokerWithDirpaths - Arg[1]=qpid.home_dir=qpid_broker_home/
 [java] 5467    INFO [FelixDispatchQueue] org.friendularity.bundle.qpid_broker_wrap.QPidBrokerLauncher (QPidBrokerLauncher.java:104) launchBrokerWithDirpaths - Arg[2]=-prop
 [java] 5467    INFO [FelixDispatchQueue] org.friendularity.bundle.qpid_broker_wrap.QPidBrokerLauncher (QPidBrokerLauncher.java:104) launchBrokerWithDirpaths - Arg[3]=qpid.work_dir=qpid_broker_work/
 [java] 5467    INFO [FelixDispatchQueue] org.friendularity.bundle.qpid_broker_wrap.QPidBrokerLauncher (QPidBrokerLauncher.java:104) launchBrokerWithDirpaths - Arg[4]=--initial-config-path
 [java] 5467    INFO [FelixDispatchQueue] org.friendularity.bundle.qpid_broker_wrap.QPidBrokerLauncher (QPidBrokerLauncher.java:104) launchBrokerWithDirpaths - Arg[5]=qpid_broker_home/our-initial-config.json
 [java] 5468    INFO [FelixDispatchQueue] org.friendularity.bundle.qpid_broker_wrap.QPidBrokerLauncher (QPidBrokerLauncher.java:104) launchBrokerWithDirpaths - Arg[6]=-prop
 [java] 5468    INFO [FelixDispatchQueue] org.friendularity.bundle.qpid_broker_wrap.QPidBrokerLauncher (QPidBrokerLauncher.java:104) launchBrokerWithDirpaths - Arg[7]=qpid.initial_config_virtualhost_config={ "type" : "Memory" }
 */
public class QPidBrokerLauncher extends BasicDebugger {
	private static Logger theLog = getLoggerForClass(QPidBrokerLauncher.class);

	static private String OPT_PROP = "-prop";

	// http://grepcode.com/file/repo1.maven.org/maven2/org.apache.qpid/qpid-broker/0.32/org/apache/qpid/server/Main.java
	//      private static final Option OPTION_INITIAL_CONFIGURATION_PATH = OptionBuilder.withArgName("path").hasArg()
	//             .withDescription("set the location of initial JSON config to use when creating/overwriting a broker configuration store")
	// 				.withLongOpt("initial-config-path").create("icp");
	static private String OPT_INIT_CONF_PATH = "--initial-config-path";

	static public String DEFAULT_HOME_DIR_PATH = "qpid_broker_home/";
	static public String DEFAULT_WORK_DIR_PATH = "qpid_broker_work/";

	static public String DEFAULT_INITIAL_CONF_PATH = DEFAULT_HOME_DIR_PATH + "our-initial-config.json";

	static public String DEFAULT_VHN_CONF = "{ \"type\" : \"Memory\" }";

	// http://grepcode.com/file/repo1.maven.org/maven2/org.apache.qpid/qpid-broker-core/0.32/org/apache/qpid/server/BrokerOptions.java
	// defines:
	// public static final String QPID_WORK_DIR  = "qpid.work_dir";
	// public static final String QPID_HOME_DIR  = "qpid.home_dir";

	static private String QPID_HOME_PROP_NAME = BrokerOptions.QPID_HOME_DIR; // "qpid.home_dir"
	static private String QPID_WORK_PROP_NAME = BrokerOptions.QPID_WORK_DIR; // "qpid.work_dir"

	// https://apache.googlesource.com/qpid-java/+/refs/heads/trunk/broker-core/src/main/java/org/apache/qpid/server/model/VirtualHostNode.java
	//  String QPID_INITIAL_CONFIG_VIRTUALHOST_CONFIG_VAR = "qpid.initial_config_virtualhost_config";

	static private String QPID_VHN_PROP_NAME  = VirtualHostNode.QPID_INITIAL_CONFIG_VIRTUALHOST_CONFIG_VAR;

	public static boolean launchBrokerWithDfltArgs(BundleContext bunCtx) {
		return launchBrokerWithDirpaths(bunCtx, DEFAULT_HOME_DIR_PATH, DEFAULT_WORK_DIR_PATH,
				DEFAULT_INITIAL_CONF_PATH);
	}

	public static boolean launchBrokerWithDirpaths(BundleContext bunCtx,
				String homeDirPath, String workDirPath, String initialConfPath) {
		boolean successFlag = false;
		// This arg setup could  be done instead through System.Properties or env vars.
		String[] pseudoArgs = {

				OPT_PROP,  				QPID_HOME_PROP_NAME + "=" + homeDirPath,
				OPT_PROP,  				QPID_WORK_PROP_NAME + "=" + workDirPath,
				OPT_PROP,  				QPID_VHN_PROP_NAME + "=" + DEFAULT_VHN_CONF,
				OPT_INIT_CONF_PATH, 	initialConfPath
		};

		ClassLoader savedCL = Thread.currentThread().getContextClassLoader();
		ClassLoader bundleCL = org.apache.qpid.server.Broker.class.getClassLoader();
		try {
			if (bundleCL != null) {
				theLog.info("\n====\nSaved old classloader={}\nSetting contextClassLoader to: {}", savedCL, bundleCL);
				Thread.currentThread().setContextClassLoader(bundleCL);
			}
			for (int i = 0; i < pseudoArgs.length; i++) {
				theLog.info("Arg[{}]={}", i, pseudoArgs[i]);
			}
			theLog.info("Launching broker main");
			successFlag = launchBrokerMain(pseudoArgs);
			theLog.info("Finished launching broker main, successFlag={}", successFlag);
		} catch (Throwable th) {
			// Usually exceptions are caught and printed farther down.
			theLog.error("QPidBrokerLauncher.launchBrokerWithDirpaths() caught exception: ", th);
			th.printStackTrace();
		} finally {
			theLog.info("\n==========\nSetting contextClassLoader back to: {}", savedCL);
			Thread.currentThread().setContextClassLoader(savedCL);
		}
		theLog.debug("launchBrokerWithDirpaths() is returning successFlag={}", successFlag);
		return successFlag;

	}
	static public boolean launchBrokerMain(String[] args) throws Throwable {
		// Our class derived from Main addresses issue #3 in comments at top, shutting off
		// QPid's configuration of Log4J.  Also addresses #4, by making process-shutdown optional
		// on failure.

		Logger brokerLogger = getLoggerForClass(QPidBrokerMain.class);
		return QPidBrokerMain.ourMainMethod(brokerLogger, args);

		// Source of Main.java is here in the top qpid-broker project.
		// http://grepcode.com/file/repo1.maven.org/maven2/org.apache.qpid/qpid-broker/0.32/org/apache/qpid/server/Main.java?av=f

		// Source of Broker.java and BrokerOptions.java is in the qpid-broker-core project.
		// http://grepcode.com/file/repo1.maven.org/maven2/org.apache.qpid/qpid-broker-core/0.32/org/apache/qpid/server/Broker.java?av=f


	}


}
