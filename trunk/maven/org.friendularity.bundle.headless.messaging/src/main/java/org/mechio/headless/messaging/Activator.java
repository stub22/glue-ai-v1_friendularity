package org.mechio.headless.messaging;

import java.util.logging.Logger;
import javax.jms.Connection;
import org.jflux.api.messaging.rk.services.ServiceCommandFactory;
import org.jflux.impl.messaging.rk.utils.ConnectionManager;
import org.jflux.impl.messaging.rk.utils.ConnectionUtils;
import org.jflux.impl.messaging.services.PortableServiceCommand;
import org.jflux.impl.services.rk.lifecycle.utils.SimpleLifecycle;
import org.jflux.impl.services.rk.osgi.lifecycle.OSGiComponent;
import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;

import static org.jflux.impl.messaging.rk.utils.ConnectionUtils.*;

public class Activator implements BundleActivator {
    private final static Logger theLogger = 
            Logger.getLogger(Activator.class.getName());
    
    @Override
    public void start(BundleContext context) throws Exception {
        //connectMotion(context);
        connectSpeech(context);
        connectAnimation(context);
        connectVision(context);
        new OSGiComponent(context, 
                new SimpleLifecycle(new PortableServiceCommand.Factory(), 
                        ServiceCommandFactory.class)).start();
    }
    
    private void connectMotion(BundleContext context) throws Exception {
        theLogger.info("Registering Motion Connection and Destinations");
        Connection con = ConnectionManager.createConnection(
                "admin", "admin", "client1", "test", 
                "tcp://127.0.0.1:5672");
        con.start();
        ConnectionUtils.ensureSession(context, 
                "motionConnection", con, null);
        ConnectionUtils.ensureDestinations(context, 
                "robotRequest", "robotRequest", TOPIC, null, 
                "robotResponse", "robotResponse", TOPIC, null,
                "robotMotionFrame", "robotMotionFrame", TOPIC, null);
        theLogger.info("Motion Connection and Destinations Registered");
    }
    
    private void connectSpeech(BundleContext context) throws Exception {
        Connection con = ConnectionManager.createConnection(
                "admin", "admin", "client1", "test", 
                "tcp://127.0.0.1:5672");
        con.start();
        theLogger.info("Registering Speech Connection and Destinations");
        ConnectionUtils.ensureSession(context, 
                "speechConnection", con, null);
        ConnectionUtils.ensureDestinations(context, 
                "speechCommand", "speechCommand", QUEUE, null, 
                "speechConfig", "speechCommand", QUEUE, null, 
                "speechError", "speechError", TOPIC, null,
                "speechRequest", "speechRequest", QUEUE, null,
                "speechEvent", "speechEvent", TOPIC, null);
        theLogger.info("Speech Connection and Destinations Registered");
    }
    
    private void connectAnimation(BundleContext context) throws Exception {
        Connection con = ConnectionManager.createConnection(
                "admin", "admin", "client1", "test", 
                "tcp://127.0.0.1:5672");
        con.start();
        theLogger.info("Registering Animation Connection and Destinations");
        ConnectionUtils.ensureSession(context, 
                "animationConnection", con, null);
        ConnectionUtils.ensureDestinations(context, 
                "animationRequest", "animationRequest", TOPIC, null);
        theLogger.info("Animation Connection and Destinations Registered");
    }
    
    private void connectVision(BundleContext context) throws Exception {
        Connection con = ConnectionManager.createConnection(
                "admin", "admin", "client1", "test", 
                "tcp://127.0.0.1:5672");
        con.start();
        theLogger.info("Registering Vision Connection and Destinations");
        ConnectionUtils.ensureSession(context, 
                "visionConnection", con, null);
        ConnectionUtils.ensureDestinations(context, 
                "visionCameraCommand", "camera0Command", QUEUE, null, 
                "visionCameraError", "camera0Error", TOPIC, null,
                "visionCameraEvent", "camera0Event", TOPIC, null, 
                "visionProcCommand", "visionproc0Command", QUEUE, null, 
                "visionProcError", "visionproc0Error", TOPIC, null,
                "visionProcEvent", "visionproc0Event", TOPIC, null);
        theLogger.info("Vision Connection and Destinations Registered");
    }

    @Override
    public void stop(BundleContext context) throws Exception {
        // TODO add deactivation code here
    }

}
