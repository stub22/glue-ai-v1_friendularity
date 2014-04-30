package org.friendularity.bundle.netconfig;


import org.cogchar.impl.netconf.NetworkServiceLauncher;
import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;

public class Activator implements BundleActivator {

	public void start(BundleContext context) throws Exception {
		// The "netconfig" functionality has moved to Cogchar.
		// This bundle will be deleted.
		// This startup should now be done from application bundles.
		NetworkServiceLauncher.startCogCharAmbassadors(context);
	}

	public void stop(BundleContext context) throws Exception {
		// TODO add deactivation code here
	}
}
