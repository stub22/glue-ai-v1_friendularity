package org.friendularity.bundle.headless.animation;

import java.util.logging.Logger;
import javax.jms.Connection;

import org.appdapter.osgi.core.BundleActivatorBase;
import org.jflux.impl.messaging.rk.lifecycle.JMSAvroAsyncReceiverLifecycle;
import org.jflux.impl.messaging.rk.lifecycle.JMSAvroMessageSenderLifecycle;
import org.jflux.impl.messaging.rk.utils.ConnectionManager;
import org.jflux.impl.messaging.rk.utils.ConnectionUtils;
import org.jflux.impl.services.rk.lifecycle.ManagedService;
import org.jflux.impl.services.rk.osgi.OSGiUtils;
import org.jflux.impl.services.rk.osgi.lifecycle.OSGiComponent;
import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;
import org.osgi.framework.ServiceRegistration;
import org.mechio.api.animation.library.AnimationLibrary;
import org.mechio.api.animation.library.DefaultAnimationLibrary;
import org.mechio.api.animation.lifecycle.AnimationPlayerHostLifecycle;
import org.mechio.api.animation.protocol.AnimationEvent;
import org.mechio.api.animation.protocol.AnimationSignal;
import org.mechio.impl.animation.messaging.AnimationEventRecord;
import org.mechio.impl.animation.messaging.AnimationSignallingRecord;
import org.mechio.impl.animation.messaging.PortableAnimationEvent;
import org.mechio.impl.animation.messaging.PortableAnimationSignal;

public class Activator extends BundleActivatorBase {
    private final static Logger theLogger = 
            Logger.getLogger(Activator.class.getName());
    public final static String PLAYER_ID = "myRobot";
    public final static String ANIM_RECEIVER_ID = "animationReceiver";
    public final static String CONNECTION_ID = "animationConnection";
    public final static String ANIM_DEST_ID = "animationRequest";
    public final static String SIGNAL_DEST_ID = "animationSignal";
    public final static String LIBRARY_ID = "myRobot";
    public final static String SIGNAL_SENDER_ID = "signalSender";
    
    
    @Override
    public void start(BundleContext context) throws Exception {
		super.start(context);
		getLogger().warn("o.f.b.headless.animation Activator.start() no longer does any service launching.");
	}

	public void doLaunchServices(BundleContext context) throws Exception {
        connectAnimation(context);
        launchRemotePlayer(context, PLAYER_ID, ANIM_RECEIVER_ID,
                SIGNAL_SENDER_ID, CONNECTION_ID, ANIM_DEST_ID);
        new OSGiComponent(context, new AnimationPlayerHostLifecycle(
                "Avatar_ZenoR50", ANIM_RECEIVER_ID, SIGNAL_SENDER_ID)
        ).start();
        launchAnimationLibrary(context, LIBRARY_ID);
    }
    
    private void connectAnimation(BundleContext context) throws Exception {
        Connection con = ConnectionManager.createConnection(
                "admin", "admin", "client1", "test", 
                "tcp://127.0.0.1:5672");
        con.start();
        theLogger.info("Registering Animation Connection and Destinations");
        ConnectionUtils.ensureSession(context, 
                CONNECTION_ID, con, null);
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

    @Override
    public void stop(BundleContext context) throws Exception {
        super.stop(context);
    }

}
