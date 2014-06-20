package org.friendularity.bundle.vision;

import org.appdapter.osgi.core.BundleActivatorBase;
import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;

public class Activator extends BundleActivatorBase {

    public void start(BundleContext context) throws Exception {
		forceLog4jConfig();
		VisionDemoFrame vdf = new VisionDemoFrame();
		vdf.setup();
		vdf.pack();
		vdf.setVisible(true);	
    }

    public void stop(BundleContext context) throws Exception {
        // TODO add deactivation code here
    }

}
