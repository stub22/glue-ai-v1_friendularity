package org.friendularity.bundle.repo;

import org.cogchar.bind.lift.LifterLifecycle;
import org.cogchar.bundle.app.puma.PumaBooter;
import org.cogchar.bundle.app.puma.PumaWebMapper;
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
		PumaBooter.startQueryService(context);
		ClassLoader hrkContentCL = com.hrkind.content.preview.PreviewContentBundleActivator.class.getClassLoader();
		PumaWebMapper pwm = new PumaWebMapper();
		pwm.connectLiftInterface(context);
		pwm.connectHrkindWebContent(hrkContentCL);
		
		// Tell the lifter lifecycle to start, once its dependencies are satisfied
		LifterLifecycle lifecycle = new LifterLifecycle();
    	OSGiComponent lifterComp = new OSGiComponent(context, lifecycle);
    	lifterComp.start();
	}
}
