package org.friendularity.bundle.headless.animation;


import javax.jms.Connection;

import org.appdapter.osgi.core.BundleActivatorBase;

import org.osgi.framework.BundleContext;

public class Activator extends BundleActivatorBase {

    @Override
    public void start(BundleContext context) throws Exception {
		super.start(context);
		getLogger().warn("o.f.b.headless.animation Activator.start() no longer does any service launching.");
	}

    @Override
    public void stop(BundleContext context) throws Exception {
        super.stop(context);
    }

}
