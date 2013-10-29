package org.friendularity.demo.lifecycles;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import org.jflux.api.registry.Descriptor;
import org.jflux.api.registry.basic.BasicDescriptor;
import org.jflux.api.service.ServiceManager;
import org.jflux.api.service.binding.ServiceBinding;
import org.jflux.impl.registry.OSGiRegistry;
import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;
import org.jflux.impl.services.rk.lifecycle.utils.ManagedServiceFactory;
import org.jflux.impl.services.rk.osgi.lifecycle.OSGiComponentFactory;
import org.robokind.api.messaging.Constants;
import org.robokind.api.messaging.services.ServiceCommand;
import org.robokind.api.messaging.services.ServiceError;
import org.robokind.api.motion.Robot;
import org.robokind.api.speech.SpeechConfig;
import org.robokind.api.speech.SpeechEventList;
import org.robokind.api.speech.SpeechRequest;
import org.robokind.api.speech.SpeechService;
import org.robokind.impl.messaging.config.RKMessagingConfigUtils;
import org.robokind.impl.speech.RemoteSpeechUtils;
import org.robokind.integration.motion_speech.VisemeMotionUtils;
import org.rwshop.swing.common.lifecycle.ServicesFrame;

public class Activator implements BundleActivator {
    
    public final static String SPEECH_DEFAULT_PREFIX = "speech";

    public void start(BundleContext context) throws Exception {
        OSGiComponentFactory factory = new OSGiComponentFactory(context);
        String speechConnectionConfigId = "speechServiceConnectionConfig";
        String speechServiceId = "speechService_01";
        String robotId = "myRobot01";
        String ipAddress = "127.0.0.1";
        String visemeConfigPath = "VisemeConf.json";
        startServiceFrame(context);
        startSpeech(factory, speechConnectionConfigId, ipAddress, speechServiceId);
        startVisemes(factory, robotId, speechServiceId, visemeConfigPath);
        
        // Configure the bindings for the RemoteSpeechServiceLifecycle
        RemoteSpeechServiceLifecycle l = new RemoteSpeechServiceLifecycle();
        Map<String, ServiceBinding> bindings = new HashMap<String, ServiceBinding>();
        
        int i = 0;
        Map<String,String> props = new HashMap<String, String>();
        props.put(Constants.PROP_MESSAGE_SENDER_ID, "speechService_01/RKSpeechGroup/speechCommand/RKMessagingGroup/remoteNotifier");
        props.put(Constants.PROP_MESSAGE_TYPE, ServiceCommand.class.getName());
        Descriptor d = new BasicDescriptor(l.getServiceDependencys().get(i).getDependencyClassName(), props);
        bindings.put(l.getServiceDependencys().get(i).getDependencyName(), new ServiceBinding(l.getServiceDependencys().get(i), d, ServiceBinding.BindingStrategy.LAZY));
        
        i++;
        props = new HashMap<String, String>();
        props.put(Constants.PROP_MESSAGE_SENDER_ID, "speechService_01/RKSpeechGroup/speechConfig/RKMessagingGroup/remoteNotifier");
        props.put(Constants.PROP_MESSAGE_TYPE, SpeechConfig.class.getName());
        d = new BasicDescriptor(l.getServiceDependencys().get(i).getDependencyClassName(), props);
        bindings.put(l.getServiceDependencys().get(i).getDependencyName(), new ServiceBinding(l.getServiceDependencys().get(i), d, ServiceBinding.BindingStrategy.LAZY));
        
        i++;
        props = new HashMap<String, String>();
        props.put(Constants.PROP_MESSAGE_RECEIVER_ID, "speechService_01/RKSpeechGroup/speechError/RKMessagingGroup/remoteListener");
        props.put(Constants.PROP_MESSAGE_TYPE, ServiceError.class.getName());
        d = new BasicDescriptor(l.getServiceDependencys().get(i).getDependencyClassName(), props);
        bindings.put(l.getServiceDependencys().get(i).getDependencyName(), new ServiceBinding(l.getServiceDependencys().get(i), d, ServiceBinding.BindingStrategy.LAZY));
        
        i++;
        i++;
        props = new HashMap<String, String>();
        props.put(Constants.PROP_MESSAGE_SENDER_ID, "speechService_01/RKSpeechGroup/speechRequest/RKMessagingGroup/remoteNotifier");
        props.put(Constants.PROP_MESSAGE_TYPE, SpeechRequest.class.getName());
        d = new BasicDescriptor(l.getServiceDependencys().get(i).getDependencyClassName(), props);
        bindings.put(l.getServiceDependencys().get(i).getDependencyName(), new ServiceBinding(l.getServiceDependencys().get(i), d, ServiceBinding.BindingStrategy.LAZY));
        
        i++;
        props = new HashMap<String, String>();
        props.put(Constants.PROP_MESSAGE_RECEIVER_ID, "speechService_01/RKSpeechGroup/speechEvent/RKMessagingGroup/remoteListener");
        props.put(Constants.PROP_MESSAGE_TYPE, SpeechEventList.class.getName());
        d = new BasicDescriptor(l.getServiceDependencys().get(i).getDependencyClassName(), props);
        bindings.put(l.getServiceDependencys().get(i).getDependencyName(), new ServiceBinding(l.getServiceDependencys().get(i), d, ServiceBinding.BindingStrategy.LAZY));
        
        ServiceManager m = new ServiceManager(l, bindings, Collections.EMPTY_MAP, null);
        
        // Configue the bindings for the VisemeEventNotifierLifecycle
        VisemeEventNotifierLifecycle l2 = new VisemeEventNotifierLifecycle();
        bindings = new HashMap<String, ServiceBinding>();
        
        i = 0;
        props = new HashMap<String, String>();
        props.put(SpeechService.PROP_ID, "speechService_01");
//        props.put(Constants.PROP_MESSAGE_TYPE, SpeechService.class.getName());
        d = new BasicDescriptor(l2.getServiceDependencys().get(i).getDependencyClassName(), props);
        bindings.put(l2.getServiceDependencys().get(i).getDependencyName(), new ServiceBinding(l2.getServiceDependencys().get(i), d, ServiceBinding.BindingStrategy.LAZY));
        
        ServiceManager m2 = new ServiceManager(l2, bindings, Collections.EMPTY_MAP, null);
        
        
        // Configue the bindings for the VisemeBindingManagerLifecycle
        VisemeBindingManagerLifecycle l3 = new VisemeBindingManagerLifecycle();
        bindings = new HashMap<String, ServiceBinding>();
        
        i = 0;
        props = new HashMap<String, String>();
//        props.put(Constants.PROP_MESSAGE_SENDER_ID, "speechService_01");
//        props.put(Constants.PROP_MESSAGE_TYPE, SpeechService.class.getName());
        d = new BasicDescriptor(l3.getServiceDependencys().get(i).getDependencyClassName(), props);
        bindings.put(l3.getServiceDependencys().get(i).getDependencyName(), new ServiceBinding(l3.getServiceDependencys().get(i), d, ServiceBinding.BindingStrategy.LAZY));
        
        ServiceManager m3 = new ServiceManager(l3, bindings, Collections.EMPTY_MAP, null);
        
        /*
        OSGiRegistry o = new OSGiRegistry(context);
        m.start(o);
        m2.start(o);
        m3.start(o);
        //*/
        //*
        
        m.start(new OSGiRegistry(context));
        m2.start(new OSGiRegistry(context));
        m3.start(new OSGiRegistry(context));
        //*/
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
    
    public static void startSpeech( 
            ManagedServiceFactory fact, String speechConnectConfigId, 
            String ipAddress, String speechServiceId){
        RKMessagingConfigUtils.registerConnectionConfig(
                speechConnectConfigId, ipAddress, null, fact);
        RemoteSpeechUtils.connect(fact, speechServiceId, SPEECH_DEFAULT_PREFIX, speechConnectConfigId);
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
