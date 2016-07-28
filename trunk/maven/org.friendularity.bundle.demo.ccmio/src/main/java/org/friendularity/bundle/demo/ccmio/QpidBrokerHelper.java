package org.friendularity.bundle.demo.ccmio;

import org.friendularity.bundle.qpid_broker_wrap.QPidBrokerLauncher;
import org.osgi.framework.BundleContext;

/**
 * Created by Stub22 on 7/27/2016.
 */
public class QpidBrokerHelper {
	public void startBroker(BundleContext bunCtx) {
		QPidBrokerLauncher.launchBrokerWithDfltArgs(bunCtx);
	}
}
