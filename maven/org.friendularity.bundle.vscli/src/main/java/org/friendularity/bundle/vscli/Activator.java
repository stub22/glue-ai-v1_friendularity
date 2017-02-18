package org.friendularity.bundle.vscli;

import org.friendularity.netcli.vwta.RunClientTestMsgSender;
import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;

public class Activator implements BundleActivator {

    public void start(BundleContext context) throws Exception {
        // TODO add activation code here
    }

    public void stop(BundleContext context) throws Exception {
        // TODO add deactivation code here
    }

	public static void runClientTest() {
		String[] args = new String[0];
		RunClientTestMsgSender.main(args);
	}
}
