package org.friendularity.bundle.qpid_broker_wrap;

import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;

// Our goal is to activate just as much of a QPid broker as needed, which to do is perhaps a bit
// harder than we would like.  Note that when all running network svcs can use netty based
// transport such as QPid Proton and Akka, then QPid broker is not needed.
public class QpidBrokerSubsetBundleActivator implements BundleActivator {

    public void start(BundleContext context) throws Exception {

    }

    public void stop(BundleContext context) throws Exception {
        // TODO add deactivation code here
    }

}
