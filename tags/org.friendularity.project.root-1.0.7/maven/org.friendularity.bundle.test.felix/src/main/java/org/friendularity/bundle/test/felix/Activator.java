package org.friendularity.bundle.test.felix;

import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;

public class Activator implements BundleActivator {

    public void start(BundleContext context) throws Exception {
		System.out.println("In the Activator.start() method of this felix-OSGi test bundle, with no other dependencies.");
    }

    public void stop(BundleContext context) throws Exception {
		System.out.println("In the Activator.stop() method");
    }

}
