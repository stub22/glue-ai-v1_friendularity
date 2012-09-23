package org.friendularity.bundle.demo.liftoff;

import org.cogchar.bind.lift.LifterLifecycle;
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
        // Tell the lifter lifecycle to start, once its dependencies are satisfied
        LifterLifecycle lifecycle = new LifterLifecycle();
        OSGiComponent lifterComp = new OSGiComponent(context, lifecycle);
        lifterComp.start();
    }
}
