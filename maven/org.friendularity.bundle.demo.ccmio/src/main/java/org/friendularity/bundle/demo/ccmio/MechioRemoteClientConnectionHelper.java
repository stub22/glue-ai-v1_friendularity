package org.friendularity.bundle.demo.ccmio;

import org.appdapter.core.log.BasicDebugger;
/**********
import org.friendularity.bundle.headless.messaging.MessagingServiceLauncher;
import org.friendularity.bundle.headless.animation.AnimServiceLauncher;
import org.friendularity.bundle.headless.speech.SpeechServiceLauncher;
 ***********/
import org.friendularity.qpc.QpidConnMgr;
import org.friendularity.qpc.QpidConnMgrJFlux;
import org.osgi.framework.BundleContext;

import javax.jms.Connection;

/**
 * Created by Stub22 on 7/27/2016.
 *
 * Mainly these services are needed when we want lifecycle dependency wiring on the
 * client side of Qpid connections to our character's mechio services.  These lifecycles
 * and the related Qpid connection are not necessarily needed in a character service process.
 * However, they *could* be used in a more complex application, involving multiple networked
 * characters and coordinated behavior planners.
 */
public class MechioRemoteClientConnectionHelper extends BasicDebugger {
	public void startEmUp(BundleContext bunCtx) {
		/*
		QpidConnMgr qcm = new QpidConnMgrJFlux();
		qcm.startConn();
		Connection qpidConn = qcm.getConn();

		String animPlayerID = "Avatar_Sinbad";	//  "Avatar_ZenoR50";
		getLogger().info("Got qpidConn={}\n\"======================================\n, launching messaging services");
		MessagingServiceLauncher.launchServices(bunCtx, qpidConn);
		getLogger().info("====================================== launching anim services");
		AnimServiceLauncher.launchServices(bunCtx, qpidConn, animPlayerID);
		getLogger().info("====================================== launching speech services");
		SpeechServiceLauncher.launchServices(bunCtx);
		getLogger().info("====================================== Finished MechIO QPid service launches");
		*/
	}

}
