package org.friendularity.bundle.headless.speech;

import org.appdapter.osgi.core.BundleActivatorBase;
import org.osgi.framework.BundleContext;

public class Activator extends BundleActivatorBase {

	@Override
	public void start(BundleContext context) throws Exception {
		super.start(context);
		// We want these services to be activated by config.
		getLogger().warn("o.f.b.headless.speech Activator.start() no longer does any service launching.");

	}


	@Override
	public void stop(BundleContext context) throws Exception {
		super.stop(context);
	}
}
