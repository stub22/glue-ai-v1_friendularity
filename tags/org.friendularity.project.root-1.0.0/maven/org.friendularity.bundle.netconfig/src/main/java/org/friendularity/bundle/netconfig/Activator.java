package org.friendularity.bundle.netconfig;

import org.cogchar.bind.lift.LiftAmbassador;
import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;
import org.jflux.impl.services.rk.lifecycle.ServiceLifecycleProvider;
import org.jflux.impl.services.rk.lifecycle.utils.SimpleLifecycle;
import org.jflux.impl.services.rk.osgi.lifecycle.OSGiComponent;

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
