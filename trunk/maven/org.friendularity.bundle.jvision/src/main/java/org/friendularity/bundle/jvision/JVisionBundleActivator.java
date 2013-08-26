package org.friendularity.bundle.jvision;

import org.appdapter.osgi.core.BundleActivatorBase;
import org.friendularity.jvision.gui.DemoFrame;
import org.friendularity.jvision.gui.JVisionLauncher;
import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;

public class JVisionBundleActivator extends BundleActivatorBase {

	JVisionLauncher myLauncher;

	@Override public void start(BundleContext context) throws Exception {
		forceLog4jConfig();
		// Print some howdys
		super.start(context);
		scheduleFrameworkStartEventHandler(context);
	}
	public void stop(BundleContext context) throws Exception {
		// This handler is important in the case where *some other bundle* (outside JVision) is trying
		// to stop the JVM/OSGi process.  
		if (myLauncher != null) {
			getLogger().info("Activator sending requestStop() to myLauncher, in case some *other* bundle is stopping the container.");
			myLauncher.requestStop(Boolean.FALSE);
		}
		super.stop(context);
	}
	@Override protected void handleFrameworkStartedEvent(BundleContext bundleCtx) {
		getLogger().info("In OSGi framework-started callback, initialization of JVision starting");
		// How to get the cmd line args if we need them
		// String[] args = (String[])context.getArguments().get("application.args");

		// Check that our Java version is at least Java 1.6 update-32
		// Versions at u25 and earlier fail with a "can't find native-library" error.
		// 1.6.0_32
		String version = System.getProperty("java.version");

		if (version == null) {
			getLogger().error("Cannot determine java version, we need 1.6.0_32, will try to run anyway");
		} else if (!version.equals("1.6.0_32")) {
			getLogger().warn("Java version is not 1.6.0_32, Versions at u25 and "
				+ "earlier fail with a \"can't find native-library\" error.");
		}

		boolean flag_stopOSGiAfterQuitCompletes = true;
		myLauncher = new JVisionLauncher(flag_stopOSGiAfterQuitCompletes);
		boolean launchedOK = myLauncher.attemptInit();
		getLogger().info("myLauncher.attemptInit() returned: " + launchedOK);
	}


}
