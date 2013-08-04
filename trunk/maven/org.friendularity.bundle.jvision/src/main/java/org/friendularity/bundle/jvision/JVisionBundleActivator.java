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
		
		myLauncher = new JVisionLauncher();
		boolean launchedOK = myLauncher.attemptInit();
    }

    public void stop(BundleContext context) throws Exception {
        // TODO add deactivation code here
		if (myLauncher != null) {
			getLogger().info("Sending requestStop() to myLauncher.");
			myLauncher.requestStop();
			//
		}
		super.stop(context);
    }

}
