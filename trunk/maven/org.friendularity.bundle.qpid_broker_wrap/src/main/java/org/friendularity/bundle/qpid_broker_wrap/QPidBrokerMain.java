package org.friendularity.bundle.qpid_broker_wrap;

import org.apache.qpid.server.BrokerOptions;
import org.slf4j.Logger;

/**
 * Created by Stub22 on 8/1/2016.
 *
 * Our goal in deriving here is just to shutoff QPid's attempt to configure Log4J.
 *
 * Superclass source is here:
 * http://grepcode.com/file/repo1.maven.org/maven2/org.apache.qpid/qpid-broker/0.32/org/apache/qpid/server/Main.java?av=f
 *
 */
public class QPidBrokerMain extends org.apache.qpid.server.Main {

	public static Logger theLog = null;
	public static boolean doExitOnLaunchFailure = false;

	// User should call this instead of Main.main(args).
	public static void ourMainMethod(Logger logger, String[] args) {
		// As of 0.32, QPid broker  Main.main() simply calls the Main constructor, which does
		// all the actual work.  This is a problem because we can't construct our own Main
		// subclass instance without calling that superclass constructor.  Hence
		// the gymnastics seen here.
		theLog = logger;
		QPidBrokerMain qbm = new QPidBrokerMain(); // suppresses superclass constructor behavior.
		qbm.ourBetterLauncherModifiedFromSuperConstructor(args); // Pass good args, without suppression.
	}
	private static String SUPPRESSED_ARG = "SUPPRESSED_PARENT";
	public static String[] suppressionArgsForParentConstructor = {SUPPRESSED_ARG};
	public QPidBrokerMain() {
		// We *must* call a superclass constructor for this class to compile.
		// But we don't like what that constructor does, so we disable it with suppressive arg.
		super (suppressionArgsForParentConstructor);
		theLog.info("Finished suppressed-superclass constructor execution");
	}
	@Override protected boolean parseCommandline(final String[] args) {
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

	private void ourBetterLauncherModifiedFromSuperConstructor(final String[] args) {
		theLog.info("In QPidBrokerMain.ourBetterLauncherModifiedFromSuperConstructor, hooray!");
		// We do *not* call superclass constructor, because it contains the hard-shutdown hook.
		// Instead we use a modified impl here.
		if (parseCommandline(args))
		{
			try	{
				execute(); // Note that this calls the startBroker method, which we override below.
			}
			catch(Exception e)
			{
				theLog.error("QPidBrokerMain caught exception during startup", e);
				if (doExitOnLaunchFailure) {
					// Superclass does this, which we do not generally want.
					theLog.warn("Exiting VM because doExitOnLaunchFailure == true !!!");
					shutdown(1);
				}
			}
		} else {
			theLog.error("parseCommandline failed for args={}", (Object) args);
		}
		theLog.info("Finished QPidBrokerMain.ourBetterLauncherModifiedFromSuperConstructor.");
	}
	@Override protected void startBroker(final BrokerOptions options) throws Exception {
		theLog.info("In our special startBroker method, tee hee!");
		options.setSkipLoggingConfiguration(true);
		super.startBroker(options); // Herein lies the beef of regular startup.
	}
}
