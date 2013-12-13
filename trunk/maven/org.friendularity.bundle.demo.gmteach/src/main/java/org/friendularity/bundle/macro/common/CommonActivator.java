/**
 * Copyright 2013 Hanson Robokind, LLC
 */

package org.friendularity.bundle.macro.common;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;

import org.apache.log4j.PropertyConfigurator;
import org.cogchar.app.puma.boot.PumaAppContext;
import org.cogchar.app.puma.boot.PumaBooter;
import org.cogchar.app.puma.config.PumaContextMediator;
import org.cogchar.app.puma.registry.PumaGlobalPrebootInjector;
import org.osgi.framework.BundleContext;
import org.slf4j.Logger;

import ext.osgi.common.MacroBundleActivatorBase;

/*
 *
 *
 * Starting code copied from Friendularity's CCRK_DemoActivator
 *
 * @author Stu B. <www.texpedient.com>
 *  @author DMiles@logicmoo.org
 */
public abstract class CommonActivator extends MacroBundleActivatorBase {

	final public void start(BundleContext bundleCtx) throws Exception {
		super.start(bundleCtx);
	}

	public void initLogging() {

		File file = new File("log4j_hrk_dev.properties");
		//		logInfo("InstalledFileLocator resolved path[" + "" + "] in module[" + getVirtcharNBClusterDir() + "] to " + file.getAbsolutePath());
		if (!file.exists()) {
			return;
		}
		try {
			URL localURL = file.toURI().toURL();
			PropertyConfigurator.configure(localURL);
		} catch (MalformedURLException ex) {
			ex.printStackTrace();
		}
	}

	public void configPuma() {

		boolean preventOverride = false;
		PumaContextMediator mediator = injectConfigBootstrapMediators(
				m_context, preventOverride);
		if (mediator == null)
			return;

		// startPumaDemo is intended to be self-contained in time, at least insofar as it is
		// now OK for applications to start their own puma-dependent features.  So at this
		// point, an app can fire up its own goodies, and tell them to attach to
		// PUMA  behavior system.  However, the Cogchar config system is intended to be
		// sufficiently general to handle most initialization cases without help from
		// bundle activators.

		getLogger().info(EQBAR + "\nCreating PumaBooter");
		// Cheaters context is used only for our demo-specific debugging features.
		// If another mediator took over instead, then we won't try to "cheat" to make those debugging features run.

		PumaContextMediator mediator2 = PumaGlobalPrebootInjector
				.getTheInjector().getMediator();
		PumaAppContext localDemoCheatersContext = null;

		if (mediator2 instanceof CommonMediator) {
			localDemoCheatersContext = ((CommonMediator) mediator2).myDemoPACtx;
		}

		if (localDemoCheatersContext != null) {
			getLogger()
					.info("We have a cheater's Puma-App-Context, but we're not cheatin with it today");
		}
	}

	public void bootPuma() {
		PumaBooter pumaBooter = new PumaBooter();
		// Cheaters context is used only for our demo-specific debugging features.
		// If another mediator took over instead, then we won't try to "cheat" to make those debugging features run.

		PumaContextMediator mediator2 = PumaGlobalPrebootInjector
				.getTheInjector().getMediator();
		PumaAppContext localDemoCheatersContext = null;

		if (mediator2 instanceof CommonMediator) {
			localDemoCheatersContext = ((CommonMediator) mediator2).myDemoPACtx;
		}

		if (localDemoCheatersContext != null) {
			getLogger()
					.info("We have a cheater's Puma-App-Context, but we're not cheatin with it today");
		}
		// Here is where the config actually gets read and used, then the OpenGL world gets created and populated.
		// The trickiest part is that there are several caveats about classloaders used for resources.
		// Those are what the Mediator pattern is meant to address, and that's why we set those up separately, first.
		PumaBooter.BootResult bootResult = pumaBooter.bootUnderOSGi(m_context,
				mediator2);
		getLogger().info(EQBAR + "\nGot PUMA BootResult: " + bootResult);
		// Uncomment/Comment this to enable/disable our demo of the "MotionComputer" computed-animation API.
		// Currently it plays a sinusoid on the waist-turn joint.
		// TODO:  Document, start hooking up to Symja and repo-config system.
	}

	public final static String ROBOT_CONNECTION_ENV_VAR_KEY = "com.hrkind.robot.connections";
	// mimic the repo semantics we've been using in PUMA Boots
	public static final String REPO_URL_VAR = "puma.boot.config.local";
	public static final String BM_TYPE_VAR = "behavior.master.type";
	public static final String BM_TYPE_HEADLESS = "headless";
	public static final String BM_TYPE_SWING = "swing";
	public static boolean HEADLESS = false;

	protected Logger getLogger() {
		return super.getLogger();
	}

	public CommonActivator() {
		String bmEnvVar = System.getProperty(BM_TYPE_VAR,
				System.getenv(BM_TYPE_VAR));
		if (bmEnvVar != null) {
			bmEnvVar = bmEnvVar.trim();
			if (BM_TYPE_HEADLESS.equals(bmEnvVar)) {
				macroStartupSettings.putSetting("launchGUI", false);
			}
		}
	}

	final @Override
	protected void handleFrameworkStartedEvent(final BundleContext bundleCtx) {
		try {
			// Legitimate "application" processing begins now, after all bundles have loaded and
			// the whole framework has "started" as succesfully as it can.
			if (isLauncherBundle())
				launchApplication(bundleCtx);
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

	}

	protected PumaContextMediator injectConfigBootstrapMediators(
			final BundleContext context, boolean preventOverride) {
		PumaContextMediator mediatorFromLocalDisk = getMediator();
		if (mediatorFromLocalDisk == null)
			return null;
		getLogger()
				.info(EQBAR
						+ "\nSetting up two mediators: disk-based and online-sheet-based");
		// Create one or more mediators, which do not yet actually read from their sources.

		PumaGlobalPrebootInjector injector = PumaGlobalPrebootInjector
				.getTheInjector();
		// False => Do not overwrite, so any other customer mediator will get preference.
		// Our DemoMediator coded below is only used as a backup/default.
		injector.setMediator(mediatorFromLocalDisk, preventOverride);
		return mediatorFromLocalDisk;

	}

	protected PumaContextMediator getMediator() {
		return null;
	}

	protected void forceLog4jConfig() {
		super.forceLog4jConfig();
	}

	static Object clInit = new Object() {
		{
			doubug("<clinit>" + this.getClass());
			try {
				System.getProperties().store(System.out, "cliIntiTime");
			} catch (Exception e) {
			}
		}
	};

	public static boolean LAUNCH_MYSELF = true;

	public static void doubug(String message) {
		//getLoggerForClass(CommonActivator.class).warn(message);
		// new Exception(message).fillInStackTrace().printStackTrace();
	}

	/*
	// everyrthing below is a unused
	@Override public void move_from_legacy_start(final BundleContext context) {
		// Will look for log4j.properties at root of this bundle.
		// Any top-level OSGi app that wants to enable Log4J (and thereby make Jena happy, while
		// retaining the power to configure Jena's logging level) should have the dependencies
		// in our pom.xml, and call this once at startup.
		forceLog4jConfig();
		// Print some howdys
		// until 2013-04-20, this mediator method implementation was inline here.
		// now we are going to try moving it to the framework start-event handler.
		// In theory it should have no effect, because the actual loading of the config data
		// does not happen until the pumaBoot sequence begins.
		// injectConfigBootstrapMediators(context);
		// Schedule our callback to the handle method below.
		// this is no longer needed to be called scheduleFrameworkStartEventHandler(context);
	}
	*/
	@Override
	public void stop(BundleContext context) throws Exception {
		super.stop(context);
	}

	@Override
	protected void launchApplication(BundleContext bundleCtx) throws Exception {
		// TODO Auto-generated method stub

	}

}
