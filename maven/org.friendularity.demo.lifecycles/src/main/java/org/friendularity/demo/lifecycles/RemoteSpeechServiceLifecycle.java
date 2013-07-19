/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.friendularity.demo.lifecycles;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import org.jflux.api.service.ServiceDependency;
import org.jflux.api.service.ServiceLifecycle;
import org.robokind.api.messaging.MessageAsyncReceiver;
import org.robokind.api.messaging.MessageSender;
import org.robokind.api.messaging.services.ServiceCommand;
import org.robokind.api.messaging.services.ServiceCommandFactory;
import org.robokind.api.messaging.services.ServiceError;
import org.robokind.api.speech.SpeechConfig;
import org.robokind.api.speech.SpeechEventList;
import org.robokind.api.speech.SpeechRequest;
import org.robokind.api.speech.SpeechRequestFactory;
import org.robokind.api.speech.messaging.RemoteSpeechServiceClient;

/**
 *
 * @author eadsjr
 */
public class RemoteSpeechServiceLifecycle implements ServiceLifecycle<RemoteSpeechServiceClient> {
    private final static String theCommandSender = "commandSender";
    private final static String theConfigSender = "configSender";
    private final static String theErrorReceiver = "errorReceiver";   
    private final static String theCommandFactory = "commandFactory"; 
    private final static String theRequestSender = "requestSender";
    private final static String theEventsReceiver = "eventsReceiver";
    private final static String theRequestFactory = "requestFactory";
    private String myLocalServiceId = "";
    private String myRemoteServiceId = "";
    
    private final static String[] theClassNames = new String[]{RemoteSpeechServiceClient.class.getName()};
    
    private final static List<ServiceDependency> theServiceDependencys = new ArrayList<ServiceDependency>(Arrays.asList(
            new ServiceDependency(theCommandSender, MessageSender.class.getName(), ServiceDependency.Cardinality.MANDATORY_UNARY, ServiceDependency.UpdateStrategy.DYNAMIC, null),
            new ServiceDependency(theConfigSender, MessageSender.class.getName(), ServiceDependency.Cardinality.MANDATORY_UNARY, ServiceDependency.UpdateStrategy.DYNAMIC, null),
            new ServiceDependency(theErrorReceiver, MessageAsyncReceiver.class.getName(), ServiceDependency.Cardinality.MANDATORY_UNARY, ServiceDependency.UpdateStrategy.DYNAMIC, null),
            new ServiceDependency(theCommandFactory, ServiceCommandFactory.class.getName(), ServiceDependency.Cardinality.MANDATORY_UNARY, ServiceDependency.UpdateStrategy.DYNAMIC, null),
            new ServiceDependency(theRequestSender, MessageSender.class.getName(), ServiceDependency.Cardinality.MANDATORY_UNARY, ServiceDependency.UpdateStrategy.DYNAMIC, null),
            new ServiceDependency(theEventsReceiver, MessageAsyncReceiver.class.getName(), ServiceDependency.Cardinality.MANDATORY_UNARY, ServiceDependency.UpdateStrategy.DYNAMIC, null),
            new ServiceDependency(theRequestFactory, SpeechRequestFactory.class.getName(), ServiceDependency.Cardinality.MANDATORY_UNARY, ServiceDependency.UpdateStrategy.DYNAMIC, null)));
    
    public List<ServiceDependency> getServiceDependencys() {
        return theServiceDependencys;
    }

    public RemoteSpeechServiceClient createService(Map<String, Object> services) {
        MessageSender<ServiceCommand> commandSender = 
                (MessageSender)services.get(theCommandSender);
        MessageSender<SpeechConfig> configSender = 
                (MessageSender)services.get(theConfigSender);
        MessageAsyncReceiver<ServiceError> errorReceiver = 
                (MessageAsyncReceiver)services.get(theErrorReceiver);
        ServiceCommandFactory commandFactory = 
                (ServiceCommandFactory)services.get(theCommandFactory);
        MessageSender<SpeechRequest> requestSender = 
                (MessageSender)services.get(theRequestSender);
        MessageAsyncReceiver<SpeechEventList> eventsReceiver = 
                (MessageAsyncReceiver)services.get(theEventsReceiver);
        SpeechRequestFactory requestFactory = 
                (SpeechRequestFactory)services.get(theRequestFactory);
        
        return new RemoteSpeechServiceClient(
                SpeechConfig.class, myLocalServiceId, myRemoteServiceId, 
                commandSender, configSender, errorReceiver, commandFactory, 
                requestSender, eventsReceiver, requestFactory);
    }

    public RemoteSpeechServiceClient handleDependencyChange(RemoteSpeechServiceClient t, String changeType,
            String dependencyName, Object dependency, Map<String,Object> availableDependencies) {
        
        if(theCommandSender.equals(dependencyName)){
            t.setCommandSender((MessageSender)dependency);
        }else if(theConfigSender.equals(dependencyName)){
            t.setConfigSender((MessageSender)dependency);
        }else if(theErrorReceiver.equals(dependencyName)){
            t.setErrorReceiver((MessageAsyncReceiver)dependency);
        }else if(theCommandFactory.equals(dependencyName)){
            t.setCommandFactory((ServiceCommandFactory)dependency);
        }else if(theRequestSender.equals(dependencyName)){
            t.setSpeechRequestSender((MessageSender)dependency);
        }else if(theEventsReceiver.equals(dependencyName)){
            t.setSpeechEventsReceiver((MessageAsyncReceiver)dependency);
        }else if(theRequestFactory.equals(dependencyName)){
            t.setSpeechRequestFactory((SpeechRequestFactory)dependency);
        }
        return t;
    }

    public void disposeService(RemoteSpeechServiceClient t, Map<String, Object> map) {
    }

    public String[] getServiceClassNames() {
        return theClassNames;
    }

    @Override
    public List<ServiceDependency> getDependencySpecs() {
        return theServiceDependencys;
    }
}
