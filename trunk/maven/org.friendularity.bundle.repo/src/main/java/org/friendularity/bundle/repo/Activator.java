package org.friendularity.bundle.repo;

import org.cogchar.bind.lift.LifterLifecycle;
import org.cogchar.bundle.app.puma.PumaBooter;
import org.cogchar.bundle.app.puma.PumaAppContext;
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
		PumaAppContext pac = new PumaAppContext(context);
		pac.startVanillaRepoClient();
		// And now, also apply the global mode
		pac.applyGlobalConfigAndStartService();
		// ... and set our app context with PumaWebMapper, so lift can issue repo update requests
		pac.getWebMapper().connectLiftInterface(context);
		// Tell the lifter lifecycle to start, once its dependencies are satisfied
		LifterLifecycle lifecycle = new LifterLifecycle();
    	OSGiComponent lifterComp = new OSGiComponent(context, lifecycle);
    	lifterComp.start();
	}
}
