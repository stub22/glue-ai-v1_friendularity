package org.friendularity.bundle.demo.ccmio;

import org.appdapter.core.log.BasicDebugger;
import org.friendularity.bundle.headless.messaging.MessagingServiceLauncher;
import org.friendularity.bundle.headless.animation.AnimServiceLauncher;
import org.friendularity.bundle.headless.speech.SpeechServiceLauncher;
import org.jflux.impl.messaging.rk.utils.ConnectionManager;
import org.osgi.framework.BundleContext;

import javax.jms.Connection;

/**
 * Created by Stub22 on 7/27/2016.
 */
public class MechioServiceHelper extends BasicDebugger {
	public void startEmUp(BundleContext bunCtx) {
		Connection qpidConn = makeOldeTestQpidConn();
		String animPlayerID = "Avatar_Sinbad";	//  "Avatar_ZenoR50";
		getLogger().info("Got qpidConn={}\n\"======================================\n, launching messaging services");
		MessagingServiceLauncher.launchServices(bunCtx, qpidConn);
		getLogger().info("====================================== launching anim services");
		AnimServiceLauncher.launchServices(bunCtx, qpidConn, animPlayerID);
		getLogger().info("====================================== launching speech services");
		SpeechServiceLauncher.launchServices(bunCtx);
		getLogger().info("====================================== Finished MechIO QPid service launches");
	}
	private Connection makeOldeTestQpidConn() {
		Connection qpidConn = null;
		try {
			qpidConn = ConnectionManager.createConnection(
					"admin", "admin", "client1", "test",
					"tcp://127.0.0.1:5672");
			qpidConn.start();
		} catch (Throwable t) {
			getLogger().error("makeOldeTestQpidConn caught exception: {}", t);
		}
		return qpidConn;
	}

}
