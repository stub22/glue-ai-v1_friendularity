package org.friendularity.spec.connection;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jms.Destination;
import javax.jms.Session;
import org.apache.avro.Schema;
import org.jflux.api.core.Adapter;
import org.jflux.api.core.config.Configuration;
import org.jflux.api.service.ServiceDependency;
import org.jflux.api.service.ServiceLifecycle;
import org.robokind.impl.messaging.JMSAvroMessageAsyncReceiver;

import static org.jflux.impl.encode.avro.SerializationConfigUtils.*;

/**
 *
 * @author Jason G. Pallack <jgpallack@gmail.com>
 */


public class MessageAsyncReceiverLifecycle
    implements ServiceLifecycle<JMSAvroMessageAsyncReceiver> {
    private final static Logger theLogger =
            Logger.getLogger(MessageSenderLifecycle.class.getName());
    private final static String theReceiverConfiguration =
            "messageReceiverConfig";
    private final static String theReceiverSession = "messageReceiverSession";
    private final static String theReceiverDestination = "messageReceiverDest";
    
    private final static ServiceDependency[] theDependencyArray = {
          new ServiceDependency(
            theReceiverConfiguration, Configuration.class.getName(),
            ServiceDependency.Cardinality.MANDATORY_UNARY,
            ServiceDependency.UpdateStrategy.STATIC, Collections.EMPTY_MAP),
          new ServiceDependency(
            theReceiverSession, Session.class.getName(),
            ServiceDependency.Cardinality.MANDATORY_UNARY,
            ServiceDependency.UpdateStrategy.STATIC, Collections.EMPTY_MAP),
          new ServiceDependency(
            theReceiverDestination, Destination.class.getName(),
            ServiceDependency.Cardinality.MANDATORY_UNARY,
            ServiceDependency.UpdateStrategy.STATIC, Collections.EMPTY_MAP)
    };
    
    private final static String[] theClassNameArray = {
        JMSAvroMessageAsyncReceiver.class.getName()
    };
    
    @Override
    public List<ServiceDependency> getDependencySpecs() {
        return Arrays.asList(theDependencyArray);
    }

    @Override
    public JMSAvroMessageAsyncReceiver createService(
            Map<String, Object> dependencyMap) {
        Configuration config =
                (Configuration)dependencyMap.get(theReceiverConfiguration);
        Session session = (Session)dependencyMap.get(theReceiverSession);
        Destination dest =
                (Destination)dependencyMap.get(theReceiverDestination);
        
        Adapter adapter =
                (Adapter)config.getPropertyValue(CONF_DECODING_ADAPTER);
        Schema schema =
                (Schema)config.getPropertyValue(CONF_AVRO_RECORD_SCHEMA);
        Class recordClass = (Class)config.getPropertyValue(CONF_OUTPUT_CLASS);
        
        JMSAvroMessageAsyncReceiver receiver = 
                new JMSAvroMessageAsyncReceiver(
                session, dest, recordClass, schema);
        receiver.setAdapter(adapter);
        
        try {
            receiver.start();
        } catch(Exception ex) {
            theLogger.log(Level.WARNING, "Error starting MessageReciever.", ex);
            return null;
        }
        
        return receiver;
    }

    @Override
    public JMSAvroMessageAsyncReceiver handleDependencyChange(
            JMSAvroMessageAsyncReceiver service, String changeType,
            String dependencyName, Object dependency,
            Map<String, Object> availableDependencies) {
        return null;
    }

    @Override
    public void disposeService(
            JMSAvroMessageAsyncReceiver service,
            Map<String, Object> availableDependencies) {
        if(service != null) {
            service.stop();
        }
    }

    @Override
    public String[] getServiceClassNames() {
        return theClassNameArray;
    }
    
}
