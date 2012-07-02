package org.friendularity.bundle.repo;

import org.cogchar.bundle.app.puma.PumaWebMapper;
import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;

public class Activator implements BundleActivator {

	public void start(BundleContext context) throws Exception {
		initWebapp();
	}

	public void stop(BundleContext context) throws Exception {
		// TODO add deactivation code here
	}

	public void initWebapp() {
		ClassLoader hrkContentCL = com.hrkind.content.preview.PreviewContentBundleActivator.class.getClassLoader();
		PumaWebMapper pwm = new PumaWebMapper();
		pwm.connectLiftInterface();
		pwm.connectHrkindWebContent(hrkContentCL);
	}
}
