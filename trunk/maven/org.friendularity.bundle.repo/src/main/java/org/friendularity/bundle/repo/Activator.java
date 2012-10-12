package org.friendularity.bundle.repo;

import org.cogchar.bind.lift.LifterLifecycle;
import org.cogchar.bundle.app.puma.PumaAppContext;
import org.cogchar.bundle.app.puma.PumaContextMediator;
import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;
import org.robokind.api.common.osgi.lifecycle.OSGiComponent;


public class Activator implements BundleActivator {

	public void start(BundleContext context) throws Exception {
		initWebapp(context);
	}

	public void stop(BundleContext context) throws Exception {
		// TODO add deactivation code here
	}

	public void initWebapp(BundleContext context) {
		// Since we are not running PumaBooter, we must at least start the query service to get sheet-based config going
		PumaContextMediator mediator = new RepoPumaMediator();
		PumaAppContext pac = new PumaAppContext(context, mediator);
		pac.startRepositoryConfigServices();
		// ... and set our app context with PumaWebMapper, so lift can issue repo update requests
		pac.getOrMakeWebMapper().connectLiftInterface(context);
		// Tell the lifter lifecycle to start, once its dependencies are satisfied
		LifterLifecycle lifecycle = new LifterLifecycle();
    	OSGiComponent lifterComp = new OSGiComponent(context, lifecycle);
    	lifterComp.start();
	}
	
	private static class RepoPumaMediator extends PumaContextMediator {
		// Override methods to customize.
	}
}
