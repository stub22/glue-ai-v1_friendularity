package org.friendularity.bundle.jvision;

import org.friendularity.jvision.gui.DemoFrame;
import org.friendularity.jvision.gui.JVisionLauncher;
import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;

public class Activator implements BundleActivator {

	JVisionLauncher		theLauncherToStop;
	
    public void start(BundleContext context) throws Exception {
		theLauncherToStop = JVisionLauncher.attemptToStartJVision();
    }

    public void stop(BundleContext context) throws Exception {
        // TODO add deactivation code here
		if (theLauncherToStop != null) {
			theLauncherToStop.requestStop();
		}
    }

}
