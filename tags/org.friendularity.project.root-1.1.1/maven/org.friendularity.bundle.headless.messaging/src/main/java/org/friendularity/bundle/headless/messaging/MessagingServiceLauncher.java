package org.friendularity.bundle.headless.messaging;

import java.util.ArrayList;
import java.util.List;
import org.appdapter.core.log.BasicDebugger;
import org.jflux.api.messaging.rk.services.ServiceCommandFactory;
import org.jflux.impl.messaging.rk.services.PortableServiceCommand;
import org.jflux.impl.messaging.rk.utils.ConnectionManager;
import org.jflux.impl.messaging.rk.utils.ConnectionUtils;
import org.jflux.impl.services.rk.lifecycle.utils.SimpleLifecycle;
import org.jflux.impl.services.rk.osgi.lifecycle.OSGiComponent;
import org.osgi.framework.BundleContext;
import org.slf4j.Logger;

import javax.jms.Connection;

import static org.jflux.impl.messaging.rk.utils.ConnectionUtils.QUEUE;
import static org.jflux.impl.messaging.rk.utils.ConnectionUtils.TOPIC;

/**
 * Code moved here from Activator by Stub22 on 7/27/2016.
 */
public class MessagingServiceLauncher extends BasicDebugger {
	private final static Logger theLogger = getLoggerForClass(MessagingServiceLauncher.class);
    private final static List<String> theLaunchedServiceIDs = new ArrayList<String>();

	public static boolean launchServices(BundleContext bunCtx, Connection qpidConn, String robotID) {
		if (theLaunchedServiceIDs.contains(robotID)) {
			theLogger.error("Ignoring attempted duplicate launch of qpid conns for msging services with ID={}", robotID);
			return false;
		}
		try {
			MessagingServiceLauncher msl = new MessagingServiceLauncher();
			msl.doLaunchServices(bunCtx, qpidConn, robotID);
			theLaunchedServiceIDs.add(robotID);
			return true;
		}	catch (Throwable t) {
			theLogger.error("Caught exception", t);
			return false;
		}

	}
	public void doLaunchServices(BundleContext bunCtx, Connection qpidConn, String robotID) throws Throwable {
		//connectMotion_UNUSED(context);
		connectSpeech(bunCtx, qpidConn, robotID);
		connectAnimation(bunCtx, qpidConn, robotID);
		connectVision(bunCtx, qpidConn, robotID);
		new OSGiComponent(bunCtx,
				new SimpleLifecycle(new PortableServiceCommand.Factory(),
						ServiceCommandFactory.class)).start();
	}

	private void connectSpeech(BundleContext context, Connection qpidConn, String robotID) throws Exception {
		theLogger.info("Registering Speech Connection and Destinations");
		ConnectionUtils.ensureSession(context,
				robotID + "_" + "speechConnection", qpidConn, null);
		ConnectionUtils.ensureDestinations(context,
				robotID + "_" + "speechCommand", "speechCommand", QUEUE, null,
				robotID + "_" + "speechConfig", "speechCommand", QUEUE, null,
				robotID + "_" + "speechError", "speechError", TOPIC, null,
				robotID + "_" + "speechRequest", "speechRequest", QUEUE, null,
				robotID + "_" + "speechEvent", "speechEvent", TOPIC, null);
		theLogger.info("Speech Connection and Destinations Registered");
	}

	private void connectAnimation(BundleContext context, Connection qpidConn, String robotID) throws Exception {
		theLogger.info("Registering Animation Connection and Destinations");
		ConnectionUtils.ensureSession(context,
				robotID + "_" + "animationConnection", qpidConn, null);
		ConnectionUtils.ensureDestinations(context,
				robotID + "_" + "animationRequest", "animationRequest", TOPIC, null);
		theLogger.info("Animation Connection and Destinations Registered");
	}

	private void connectVision(BundleContext context, Connection qpidConn, String robotID) throws Exception {
		theLogger.info("Registering Vision Connection and Destinations");
		ConnectionUtils.ensureSession(context,
				robotID + "_" + "visionConnection", qpidConn, null);
		ConnectionUtils.ensureDestinations(context,
				robotID + "_" + "visionCameraCommand", "camera0Command", QUEUE, null,
				robotID + "_" + "visionCameraError", "camera0Error", TOPIC, null,
				robotID + "_" + "visionCameraEvent", "camera0Event", TOPIC, null,
				robotID + "_" + "visionProcCommand", "visionproc0Command", QUEUE, null,
				robotID + "_" + "visionProcError", "visionproc0Error", TOPIC, null,
				robotID + "_" + "visionProcEvent", "visionproc0Event", TOPIC, null);
		theLogger.info("Vision Connection and Destinations Registered");
	}

}
