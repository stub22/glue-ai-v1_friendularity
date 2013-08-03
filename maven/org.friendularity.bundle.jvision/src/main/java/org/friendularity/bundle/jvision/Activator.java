package org.friendularity.bundle.jvision;

import org.friendularity.jvision.gui.DemoFrame;
import org.friendularity.jvision.gui.JVisionLauncher;
import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;

public class Activator implements BundleActivator {

    public void start(BundleContext context) throws Exception {
        // TODO add activation code here
		// DemoFrame dframe = new DemoFrame();
		JVisionLauncher.main(new String[0]);
    }

    public void stop(BundleContext context) throws Exception {
        // TODO add deactivation code here
    }

}
