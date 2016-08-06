package org.friendularity.bundle.demo.ccmio;

import org.appdapter.core.log.BasicDebugger;
import org.friendularity.bundle.headless.messaging.MessagingServiceLauncher;
import org.friendularity.bundle.headless.animation.AnimServiceLauncher;
import org.friendularity.bundle.headless.speech.SpeechServiceLauncher;
import org.friendularity.qpc.JFluxQpidConnHelp;
import org.osgi.framework.BundleContext;

import javax.jms.Connection;

/**
 * Created by Stub22 on 7/27/2016.
 */
public class MechioServiceHelper extends BasicDebugger {
	public void startEmUp(BundleContext bunCtx) {
		JFluxQpidConnHelp qch = new JFluxQpidConnHelp();
		Connection qpidConn = qch.makeOldeDfltLocalTestQpidConn();
		String animPlayerID = "Avatar_Sinbad";	//  "Avatar_ZenoR50";
		getLogger().info("Got qpidConn={}\n\"======================================\n, launching messaging services");
		MessagingServiceLauncher.launchServices(bunCtx, qpidConn);
		getLogger().info("====================================== launching anim services");
		AnimServiceLauncher.launchServices(bunCtx, qpidConn, animPlayerID);
		getLogger().info("====================================== launching speech services");
		SpeechServiceLauncher.launchServices(bunCtx);
		getLogger().info("====================================== Finished MechIO QPid service launches");
	}
	/*
	private Connection makeOldeDfltLocalTestQpidConn() {
		Connection qpidConn = null;
		String qpidUsr = "admin";
		String qpidPsw = "admin";
		String qpidCliName = "client1";  // TODO:  Randomize?
		String qpidVHostName = "test"; // == default from QPid 0.26 and previous
		String qpidTcpUrl = "tcp://127.0.0.1:5672";
		try {
			qpidConn = ConnectionManager.createConnection(qpidUsr, qpidPsw, qpidCliName, qpidVHostName, qpidTcpUrl);
			qpidConn.start();
		} catch (Throwable t) {
			getLogger().error("makeOldeDfltLocalTestQpidConn caught exception: {}", t);
		}
		return qpidConn;
	}
	*/

}
