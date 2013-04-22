package org.friendularity.demo.lifecycles;

import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;
import org.robokind.api.common.lifecycle.utils.ManagedServiceFactory;
import org.robokind.api.common.osgi.lifecycle.OSGiComponentFactory;
import org.robokind.api.motion.Robot;
import org.robokind.impl.messaging.config.RKMessagingConfigUtils;
import org.robokind.impl.speech.RemoteSpeechUtils;
import org.robokind.integration.motion_speech.VisemeMotionUtils;
import org.rwshop.swing.common.lifecycle.ServicesFrame;

public class Activator implements BundleActivator {

    public void start(BundleContext context) throws Exception {
        OSGiComponentFactory factory = new OSGiComponentFactory(context);
        String speechConnectionConfigId = "speechServiceConnectionConfig";
        String speechServiceId = "speechService_01";
        String robotId = "myRobot01";
        String ipAddress = "127.0.0.1";
        String visemeConfigPath = "VisemeConf.json";
        startServiceFrame(context);
        startSpeech(context, factory, speechConnectionConfigId, ipAddress, speechServiceId);
        startVisemes(factory, robotId, speechServiceId, visemeConfigPath);
    }
    
    static void startServiceFrame(final BundleContext context){
        try{
            UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
        }catch(Exception ex){}
        SwingUtilities.invokeLater(new Runnable() {

            public void run() {
                ServicesFrame frame = new ServicesFrame();
                frame.setBundleContext(context);
                frame.setVisible(true);
            }
        });
    }
    
    public static void startSpeech(BundleContext context, 
            ManagedServiceFactory fact, String speechConnectConfigId, 
            String ipAddress, String speechServiceId){
        RKMessagingConfigUtils.registerConnectionConfig(
                speechConnectConfigId, ipAddress, null, fact);
        RemoteSpeechUtils.connect(context, speechServiceId, speechConnectConfigId);
    }
    
    public static void startVisemes(
            ManagedServiceFactory fact, String robotId, 
            String speechServiceId, String visemeConfigPath){
        VisemeMotionUtils.startVisemeFrameSourceGroup(fact,
                new Robot.Id(robotId), speechServiceId, visemeConfigPath);
    }

    public void stop(BundleContext context) throws Exception {
        // TODO add deactivation code here
    }

}
