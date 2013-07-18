package org.friendularity.spec.connection;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jms.Connection;
import javax.jms.JMSException;
import javax.jms.Session;
import org.jflux.api.service.ServiceDependency;
import org.jflux.api.service.ServiceLifecycle;

/**
 *
 * @author Jason G. Pallack <jgpallack@gmail.com>
 */


public class SessionLifecycle implements ServiceLifecycle<Session> {
    private final static Logger theLogger =
            Logger.getLogger(SessionLifecycle.class.getName());
    private final static String theSessionConnection = "sessionConnection";
    
    private final static ServiceDependency[] theDependencyArray = {
        new ServiceDependency(
            theSessionConnection, Connection.class.getName(),
            ServiceDependency.Cardinality.MANDATORY_UNARY,
            ServiceDependency.UpdateStrategy.STATIC, Collections.EMPTY_MAP)
    };
    
    private final static String[] theClassNameArray = {
        Session.class.getName()
    };
    
    @Override
    public List<ServiceDependency> getDependencySpecs() {
        return Arrays.asList(theDependencyArray);
    }

    @Override
    public Session createService(Map<String, Object> dependencyMap) {
        Connection conn = (Connection)dependencyMap.get(theSessionConnection);
        
        try {
            return conn.createSession(false, Session.CLIENT_ACKNOWLEDGE);
        } catch(JMSException ex) {
            theLogger.log(Level.WARNING, "Error starting Session.", ex);
            return null;
        }
    }

    @Override
    public Session handleDependencyChange(
            Session service, String changeType, String dependencyName,
            Object dependency, Map<String, Object> availableDependencies) {
        return null;
    }

    @Override
    public void disposeService(
            Session service, Map<String, Object> availableDependencies) {
        try {
            service.close();
        } catch(JMSException ex) {
            theLogger.log(Level.WARNING, "Error stopping Session.", ex);
        }
    }

    @Override
    public String[] getServiceClassNames() {
        return theClassNameArray;
    }
    
}
