package org.friendularity.bundle.headless.messaging;

import javax.jms.Connection;

import org.appdapter.osgi.core.BundleActivatorBase;
import org.jflux.api.messaging.rk.services.ServiceCommandFactory;
import org.jflux.impl.messaging.rk.utils.ConnectionManager;
import org.jflux.impl.messaging.rk.utils.ConnectionUtils;
import org.jflux.impl.messaging.rk.services.PortableServiceCommand;
import org.jflux.impl.services.rk.lifecycle.utils.SimpleLifecycle;
import org.jflux.impl.services.rk.osgi.lifecycle.OSGiComponent;
import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;
import org.slf4j.Logger;

import static org.jflux.impl.messaging.rk.utils.ConnectionUtils.*;

public class Activator extends BundleActivatorBase {

	@Override
	public void start(BundleContext context) throws Exception {
		super.start(context);
		getLogger().warn("o.f.b.headless.messaging Activator.start() no longer does any service launching.");
	}


    @Override
    public void stop(BundleContext context) throws Exception {
        // TODO add deactivation code here
    }

}
