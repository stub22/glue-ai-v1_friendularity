package org.friendularity.spec.connection;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;
import javax.jms.Destination;
import javax.jms.Session;
import org.jflux.api.core.Adapter;
import org.jflux.api.core.config.Configuration;
import org.jflux.api.service.ServiceDependency;
import org.jflux.api.service.ServiceLifecycle;
import org.jflux.impl.messaging.rk.JMSAvroMessageSender;

import static org.jflux.impl.encode.avro.SerializationConfigUtils.*;

/**
 *
 * @author Jason G. Pallack <jgpallack@gmail.com>
 */
public class MessageSenderLifecycle
        implements ServiceLifecycle<JMSAvroMessageSender> {

    private final static Logger theLogger =
            Logger.getLogger(MessageSenderLifecycle.class.getName());
//    private final static String theSenderConfiguration = "messageSenderConfig";
//    private final static String theSenderSession = "messageSenderSession";
//    private final static String theSenderDestination = "messageSenderDest";
    private final static String theSenderConfiguration = "msg_config_dep";
    private final static String theSenderSession = "session_dep";
    private final static String theSenderDestination = "destination_dep";
    
    private final static ServiceDependency[] theDependencyArray = {
        new ServiceDependency(
        theSenderConfiguration, Configuration.class.getName(),
        ServiceDependency.Cardinality.MANDATORY_UNARY,
        ServiceDependency.UpdateStrategy.DYNAMIC, Collections.EMPTY_MAP),
        new ServiceDependency(
        theSenderSession, Session.class.getName(),
        ServiceDependency.Cardinality.MANDATORY_UNARY,
        ServiceDependency.UpdateStrategy.STATIC, Collections.EMPTY_MAP),
        new ServiceDependency(
        theSenderDestination, Destination.class.getName(),
        ServiceDependency.Cardinality.MANDATORY_UNARY,
        ServiceDependency.UpdateStrategy.STATIC, Collections.EMPTY_MAP)
    };
    private final static String[] theClassNameArray = {
        JMSAvroMessageSender.class.getName()
    };

    public MessageSenderLifecycle() {
    }

    @Override
    public List<ServiceDependency> getDependencySpecs() {
        return Arrays.asList(theDependencyArray);
    }

    @Override
    public JMSAvroMessageSender createService(
            Map<String, Object> dependencyMap) {
        Configuration config =
                (Configuration) dependencyMap.get(theSenderConfiguration);
        Session session = (Session) dependencyMap.get(theSenderSession);
        Destination dest = (Destination) dependencyMap.get(theSenderDestination);

        Adapter adapter =
                (Adapter) config.getPropertyValue(CONF_ENCODING_ADAPTER);

        JMSAvroMessageSender sender = new JMSAvroMessageSender(session, dest);
        sender.setAdapter(adapter);
        sender.start();

        return sender;
    }

    @Override
    public JMSAvroMessageSender handleDependencyChange(
            JMSAvroMessageSender service, String changeType,
            String dependencyName, Object dependency,
            Map<String, Object> availableDependencies) {
        if (!dependencyName.equals(theSenderConfiguration)) {
            return null;
        }

        if (dependency == null) {
            service.setAdapter(null);
            return service;
        }

        Configuration config = (Configuration) dependency;
        Adapter adapter =
                (Adapter) config.getPropertyValue(CONF_ENCODING_ADAPTER);
        service.setAdapter(adapter);
        return service;
    }

    @Override
    public void disposeService(
            JMSAvroMessageSender service,
            Map<String, Object> availableDependencies) {
        if (service != null) {
            service.stop();
        }
    }

    @Override
    public String[] getServiceClassNames() {
        return theClassNameArray;
    }
}
