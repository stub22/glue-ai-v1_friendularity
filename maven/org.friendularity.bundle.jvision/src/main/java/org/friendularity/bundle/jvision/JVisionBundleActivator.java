package org.friendularity.bundle.jvision;

import org.appdapter.osgi.core.BundleActivatorBase;
import org.friendularity.jvision.gui.JVisionLauncher;
import org.osgi.framework.BundleContext;
import org.slf4j.LoggerFactory;

public class JVisionBundleActivator extends BundleActivatorBase {

	JVisionLauncher myLauncher;
	
	// Set this to false to stop JVision from launching itself. 
	public static boolean LAUNCH_MYSELF = true;

	@Override public void start(BundleContext context) throws Exception {
		forceLog4jConfig();
		// Print some howdys
		super.start(context);
		scheduleFrameworkStartEventHandler(context);
	}
	@Override public void stop(BundleContext context) throws Exception {
		// This handler is important in the case where *some other bundle* (outside JVision) is trying
		// to stop the JVM/OSGi process.  
		if (myLauncher != null) {
			getLogger().info("Activator sending requestStop() to myLauncher, in case some *other* bundle is stopping the container.");
			myLauncher.requestStop(Boolean.FALSE);
		}
		super.stop(context);
	}
	public static void setLaunchFlag(boolean launchFlag) {
		LoggerFactory.getLogger(JVisionBundleActivator.class).info("Setting launchFlag to {}", launchFlag);
		LAUNCH_MYSELF = launchFlag;
	}
	@Override protected void handleFrameworkStartedEvent(BundleContext bundleCtx) {
		getLogger().info("In OSGi framework-started callback, JVision launchFlag is {}", LAUNCH_MYSELF);
		if (LAUNCH_MYSELF) {
			launchJVisionDemo();
		} else {
			getLogger().info("Aborting JVision Launch because LAUNCH_MYSELF is false");
			return;
		}
	} 
	private void launchJVisionDemo() { 
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
