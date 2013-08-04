package org.friendularity.bundle.jvision;

import org.appdapter.osgi.core.BundleActivatorBase;
import org.friendularity.jvision.gui.DemoFrame;
import org.friendularity.jvision.gui.JVisionLauncher;
import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;

public class JVisionBundleActivator  extends BundleActivatorBase {

	JVisionLauncher		myLauncher;
	
    public void start(BundleContext context) throws Exception {
		forceLog4jConfig();
		// Print some howdys
		super.start(context);		
		
		// TODO:  Check that our Java version is at least Java 1.6 update-32, and print warnings otherwise!
		// Versions at u25 and earlier fail with a "can't find native-library" error.
		boolean flag_stopOSGiAfterQuitCompletes = true;
		myLauncher = new JVisionLauncher(flag_stopOSGiAfterQuitCompletes);
		boolean launchedOK = myLauncher.attemptInit();
		getLogger().debug("myLauncher.attemptInit() returned: " + launchedOK);
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

}
