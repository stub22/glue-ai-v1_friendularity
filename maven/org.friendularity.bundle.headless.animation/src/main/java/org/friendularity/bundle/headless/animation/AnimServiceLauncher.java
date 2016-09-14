package org.friendularity.bundle.headless.animation;

import org.appdapter.core.log.BasicDebugger;
import org.jflux.impl.messaging.rk.lifecycle.JMSAvroAsyncReceiverLifecycle;
import org.jflux.impl.messaging.rk.lifecycle.JMSAvroMessageSenderLifecycle;
import org.jflux.impl.messaging.rk.utils.ConnectionUtils;
import org.jflux.impl.services.rk.lifecycle.ManagedService;
import org.jflux.impl.services.rk.osgi.OSGiUtils;
import org.jflux.impl.services.rk.osgi.lifecycle.OSGiComponent;
import org.mechio.api.animation.library.AnimationLibrary;
import org.mechio.api.animation.library.DefaultAnimationLibrary;
import org.mechio.api.animation.lifecycle.AnimationPlayerHostLifecycle;
import org.mechio.api.animation.protocol.AnimationEvent;
import org.mechio.api.animation.protocol.AnimationSignal;
import org.mechio.impl.animation.messaging.AnimationEventRecord;
import org.mechio.impl.animation.messaging.AnimationSignallingRecord;
import org.mechio.impl.animation.messaging.PortableAnimationEvent;
import org.mechio.impl.animation.messaging.PortableAnimationSignal;
import org.osgi.framework.BundleContext;
import org.osgi.framework.ServiceRegistration;
import org.slf4j.Logger;

import javax.jms.Connection;


/**
 * Code moved here from Activator by Stub22 on 7/27/2016.
 *
 * Anyone may construct and trigger this launcher, but it uses a (global) static flag to ensure that
 * launch only occurs once.
 *
 * Will need some refinement if we want to support multiple robots/characters or multiple anim-players.
 *
 * TODO:  Clarify what this class is doing!
 *
 */
public class AnimServiceLauncher extends BasicDebugger {
	private final static Logger theLogger = getLoggerForClass(AnimServiceLauncher.class);
	public final static String PLAYER_ID = "myRobot";
	public final static String ANIM_RECEIVER_ID = "animationReceiver";
	public final static String CONNECTION_ID = "animationConnection";
	public final static String ANIM_DEST_ID = "animationRequest";
	public final static String SIGNAL_DEST_ID = "animationSignal";
	public final static String LIBRARY_ID = "myRobot";
	public final static String SIGNAL_SENDER_ID = "signalSender";

	static public boolean theLaunchHasHappened = false;

	//  "Avatar_ZenoR50"
	public static boolean launchServices(BundleContext bunCtx, Connection qpidConn, String animPlayerID)  {
		if (theLaunchHasHappened) {
			theLogger.error("Ignoring attempted duplicate launch of qpid conns for animation services, animPlayer={}", animPlayerID);
			return false;
		}
		try {
			AnimServiceLauncher asl = new AnimServiceLauncher();
			asl.doLaunchServices(bunCtx, qpidConn, animPlayerID);
			theLaunchHasHappened = true;
			return true;
		}	catch (Throwable t) {
			theLogger.error("Caught exception", t);
			return false;
		}
	}

	public void doLaunchServices(BundleContext context, Connection qpidConn, String animPlayerID) throws Throwable {
		connectAnimation(context, qpidConn);
		launchRemotePlayer(context, animPlayerID, ANIM_RECEIVER_ID,
				SIGNAL_SENDER_ID, CONNECTION_ID, ANIM_DEST_ID);
		AnimationPlayerHostLifecycle animPlayerHostLcyc = new AnimationPlayerHostLifecycle(animPlayerID, ANIM_RECEIVER_ID, SIGNAL_SENDER_ID);
		OSGiComponent<AnimationPlayerHostLifecycle> osgiComp = new OSGiComponent(context, animPlayerHostLcyc);
		osgiComp.start();
		launchAnimationLibrary(context, LIBRARY_ID);
	}

	private void connectAnimation(BundleContext context, Connection qpidConn) throws Exception {
		/*
        Connection qpidConn = ConnectionManager.createConnection(
                "admin", "admin", "client1", "test",
                "tcp://127.0.0.1:5672");
        con.start();
        */
		theLogger.info("Registering Animation Connection and Destinations");
		ConnectionUtils.ensureSession(context,
				CONNECTION_ID, qpidConn, null);
		ConnectionUtils.ensureDestinations(context,
				ANIM_DEST_ID, "animationRequest", ConnectionUtils.TOPIC, null);
		ConnectionUtils.ensureDestinations(context,
				SIGNAL_DEST_ID, "animationSignal", ConnectionUtils.TOPIC, null);
		theLogger.info("Animation Connection and Destinations Registered");
	}

	private void launchRemotePlayer(BundleContext context,
									String playerId, String receiverId, String senderId, String conId, String destId){
		JMSAvroMessageSenderLifecycle signalLife =
				new JMSAvroMessageSenderLifecycle(
						new PortableAnimationSignal.MessageRecordAdapter(),
						AnimationSignal.class, AnimationSignallingRecord.class,
						senderId, CONNECTION_ID, SIGNAL_DEST_ID);
		ManagedService myAnimationSenderService =
				new OSGiComponent(context, signalLife);
		myAnimationSenderService.start();

		theLogger.info("Launching Dynamic RemoteAnimationPlayerHost Service.");
		new OSGiComponent(context,
				new AnimationPlayerHostLifecycle(playerId, receiverId, senderId)
		).start();

		JMSAvroAsyncReceiverLifecycle reqRecLifecycle =
				new JMSAvroAsyncReceiverLifecycle(
						new PortableAnimationEvent.RecordMessageAdapter(),
						AnimationEvent.class, AnimationEventRecord.class,
						AnimationEventRecord.SCHEMA$, ANIM_RECEIVER_ID,
						conId, destId);
		OSGiComponent reqRec = new OSGiComponent(context, reqRecLifecycle);
		reqRec.start();
		theLogger.info("Dynamic RemoteAnimationPlayerHost Service Launched.");
	}

	private void launchAnimationLibrary(BundleContext context, String libraryId){
		theLogger.info("Launching AnimationLibrary Service.");
		AnimationLibrary library = new DefaultAnimationLibrary(libraryId);
		ServiceRegistration reg = OSGiUtils.registerService(context,
				AnimationLibrary.class.getName(),
				AnimationLibrary.PROP_ANIM_PLAYER_ID,
				libraryId, library, null);
		theLogger.info("AnimationLibrary Service Launched.");
	}

}
