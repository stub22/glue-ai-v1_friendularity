package org.friendularity.bundle.headless.messaging;

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
import static org.jflux.impl.messaging.rk.utils.ConnectionUtils.getPassword;
import static org.jflux.impl.messaging.rk.utils.ConnectionUtils.getUsername;

/**
 * Code moved here from Activator by Stub22 on 7/27/2016.
 */
public class MessagingServiceLauncher extends BasicDebugger {
	private final static Logger theLogger = getLoggerForClass(MessagingServiceLauncher.class);

	static public boolean theLaunchHasHappened = false;

	public static boolean launchServices(BundleContext bunCtx, Connection qpidConn) {
		if (theLaunchHasHappened) {
			theLogger.error("Ignoring attempted duplicate launch of qpid conns for msging services");
			return false;
		}
		try {
			MessagingServiceLauncher msl = new MessagingServiceLauncher();
			msl.doLaunchServices(bunCtx, qpidConn);
			theLaunchHasHappened = true;
			return true;
		}	catch (Throwable t) {
			theLogger.error("Caught exception", t);
			return false;
		}

	}
	public void doLaunchServices(BundleContext bunCtx, Connection qpidConn) throws Throwable {
		//connectMotion_UNUSED(context);
		connectSpeech(bunCtx, qpidConn);
		connectAnimation(bunCtx, qpidConn);
		connectVision(bunCtx, qpidConn);
		new OSGiComponent(bunCtx,
				new SimpleLifecycle(new PortableServiceCommand.Factory(),
						ServiceCommandFactory.class)).start();
	}

	private void connectMotion_UNUSED(BundleContext bunCtx, Connection qpidConn) throws Exception {
		theLogger.info("Registering Motion Connection and Destinations");
		/*
		Connection con = ConnectionManager.createConnection(
				getUsername(), getPassword(), "client1", "test",
				"tcp://127.0.0.1:5672");
		con.start();
		*/
		ConnectionUtils.ensureSession(bunCtx,
				"motionConnection", qpidConn, null);
		ConnectionUtils.ensureDestinations(bunCtx,
				"robotRequest", "robotRequest", TOPIC, null,
				"robotResponse", "robotResponse", TOPIC, null,
				"robotMotionFrame", "robotMotionFrame", TOPIC, null);
		theLogger.info("Motion Connection and Destinations Registered");
	}

	private void connectSpeech(BundleContext context, Connection qpidConn) throws Exception {
		/*
		Connection con = ConnectionManager.createConnection(
				getUsername(), getPassword(), "client1", "test",
				"tcp://127.0.0.1:5672");
		con.start();
		*/
		theLogger.info("Registering Speech Connection and Destinations");
		ConnectionUtils.ensureSession(context,
				"speechConnection", qpidConn, null);
		ConnectionUtils.ensureDestinations(context,
				"speechCommand", "speechCommand", QUEUE, null,
				"speechConfig", "speechCommand", QUEUE, null,
				"speechError", "speechError", TOPIC, null,
				"speechRequest", "speechRequest", QUEUE, null,
				"speechEvent", "speechEvent", TOPIC, null);
		theLogger.info("Speech Connection and Destinations Registered");
	}

	private void connectAnimation(BundleContext context, Connection qpidConn) throws Exception {
		/*
		Connection con = ConnectionManager.createConnection(
				getUsername(), getPassword(), "client1", "test",
				"tcp://127.0.0.1:5672");
		con.start();
		*/
		theLogger.info("Registering Animation Connection and Destinations");
		ConnectionUtils.ensureSession(context,
				"animationConnection", qpidConn, null);
		ConnectionUtils.ensureDestinations(context,
				"animationRequest", "animationRequest", TOPIC, null);
		theLogger.info("Animation Connection and Destinations Registered");
	}

	private void connectVision(BundleContext context, Connection qpidConn) throws Exception {
		/*
		Connection con = ConnectionManager.createConnection(
				getUsername(), getPassword(), "client1", "test",
				"tcp://127.0.0.1:5672");
		con.start();
		*/
		theLogger.info("Registering Vision Connection and Destinations");
		ConnectionUtils.ensureSession(context,
				"visionConnection", qpidConn, null);
		ConnectionUtils.ensureDestinations(context,
				"visionCameraCommand", "camera0Command", QUEUE, null,
				"visionCameraError", "camera0Error", TOPIC, null,
				"visionCameraEvent", "camera0Event", TOPIC, null,
				"visionProcCommand", "visionproc0Command", QUEUE, null,
				"visionProcError", "visionproc0Error", TOPIC, null,
				"visionProcEvent", "visionproc0Event", TOPIC, null);
		theLogger.info("Vision Connection and Destinations Registered");
	}

}
