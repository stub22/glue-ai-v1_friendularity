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
import org.slf4j.Logger;

/**
 * Created by Stub22 on 8/1/2016.
 *
 * Our goal in deriving here is just to shutoff QPid's attempt to configure Log4J.
 *
 * Superclass source is here: http://grepcode.com/file/repo1.maven.org/maven2/org.apache.qpid/qpid-broker/0.32/org/apache/qpid/server/Main.java?av=f
 */
public class QPidBrokerMain extends org.apache.qpid.server.Main {

	public static Logger theLog = null;
	public static boolean doExitOnLaunchFailure = false;

	// User should call this instead of Main.main(args).
	public static boolean ourMainMethod(Logger logger, String[] args) {
		// As of 0.32, QPid broker  Main.main() simply calls the Main constructor, which does
		// all the actual work.  This is a problem because we can't construct our own Main
		// subclass instance without calling that superclass constructor.  Hence
		// the gymnastics seen here.
		theLog = logger;
		QPidBrokerMain qbm = new QPidBrokerMain(); // suppresses superclass constructor behavior.
		return qbm.ourBetterLauncherModifiedFromSuperConstructor(args); // Pass good args, without suppression.
	}

	private static String SUPPRESSED_ARG = "SUPPRESSED_PARENT";
	public static String[] suppressionArgsForParentConstructor = {SUPPRESSED_ARG};

	public QPidBrokerMain() {
		// We *must* call a superclass constructor for this class to compile.
		// But we don't like what that constructor does, so we disable it with suppressive arg.
		super(suppressionArgsForParentConstructor);
		theLog.info("Finished suppressed-superclass constructor execution");
	}

	@Override
	protected boolean parseCommandline(final String[] args) {
		// Implements the suppression trick, since this method happens to be called on first
		// line of Main's constructor.
		if ((args.length == 1) && (args[0].equals(SUPPRESSED_ARG))) {
			theLog.info("Suppressing parent constructor by returning false from parseCommandLine");
			return false;
		} else {
			theLog.info("Calling super.parseCommandLine for the subclass runner");
			return super.parseCommandline(args);
		}
	}

	private boolean ourBetterLauncherModifiedFromSuperConstructor(final String[] args) {
		boolean successFlag = false;
		theLog.info("In QPidBrokerMain.ourBetterLauncherModifiedFromSuperConstructor, hooray!");
		// We do allow superclass constructor to get past the first line, because it contains
		// the hard-shutdown exception handler.   Instead we use a modified impl here.
		if (parseCommandline(args)) {
			try {
				execute(); // Note that this calls the startBroker method, which we override below.
				successFlag = true;
			} catch (Exception e) {
				theLog.error("QPidBrokerMain caught exception during startup", e);
				weFailedSoMaybeExitJVM();
			}
		} else {
			theLog.error("parseCommandline failed for args={}", (Object) args);
			weFailedSoMaybeExitJVM();
		}
		theLog.info("QPidBrokerMain.ourBetterLauncherMod- returning successFlag={}", successFlag);
		return successFlag;
	}

	private void weFailedSoMaybeExitJVM() {
		// Superclass always does this shutdown on launch failure, but we make it optional.
		if (doExitOnLaunchFailure) {
			theLog.warn("Exiting VM because doExitOnLaunchFailure == true !!!");
			shutdown(1);
		}
	}

	@Override
	protected void startBroker(final BrokerOptions options) throws Exception {
		final boolean skipLoggingConfiguration = true;
		theLog.info("Overriding startBroker method. skipLoggingConfiguration={}", skipLoggingConfiguration);
		options.setSkipLoggingConfiguration(skipLoggingConfiguration);
		super.startBroker(options); // Herein lies the beef of regular startup.
	}
}
