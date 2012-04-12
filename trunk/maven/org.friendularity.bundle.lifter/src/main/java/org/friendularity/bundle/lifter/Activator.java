package org.friendularity.bundle.lifter;

import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;

public class Activator implements BundleActivator {

    public void start(BundleContext context) throws Exception {
        // TODO add activation code here
		System.out.println("Hello activated!");
    }

    public void stop(BundleContext context) throws Exception {
        // TODO add deactivation code here
    }

}
