package org.friendularity.bundle.netconfig;

import org.cogchar.bind.lift.LiftAmbassador;
import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;
import org.robokind.api.common.lifecycle.ServiceLifecycleProvider;
import org.robokind.api.common.lifecycle.utils.SimpleLifecycle;
import org.robokind.api.common.osgi.lifecycle.OSGiComponent;

public class Activator implements BundleActivator {

	public void start(BundleContext context) throws Exception {
		connectCogCharInterface(context);
	}

	public void stop(BundleContext context) throws Exception {
		// TODO add deactivation code here
	}

	private static void connectCogCharInterface(BundleContext context) {
		// Connect a CogCharNetworkConfigAmbassador as a (no lifecycle) managed service, so lifter can use it
		ServiceLifecycleProvider ambassadorCycle =
				new SimpleLifecycle(new CogCharNetworkConfigAmbassador(), LiftAmbassador.LiftNetworkConfigInterface.class);
		OSGiComponent cncaComp = new OSGiComponent(context, ambassadorCycle);
		cncaComp.start();
	}
}
