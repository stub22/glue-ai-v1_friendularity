/*
 * Copyright 2013 The Friendularity Project (www.friendularity.org).
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.friendularity.spec.connection;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jms.Connection;
import javax.jms.JMSException;
import org.jflux.api.service.ServiceLifecycle;
import org.apache.qpid.client.AMQConnectionFactory;
import org.apache.qpid.url.URLSyntaxException;

/**
 * This lifecycle comprises the JFlux object registry interface for the
 * Connection object. JFlux will provide any dependencies that are necessary and 
 * inform this class as they change. JFlux will also provide this object to
 * others once its dependencies are fulfilled.
 * 
 * Lifecycles are intended to be stateless, simply providing meta-code for the 
 * object to gracefully handle changes in its environment.
 * 
 * @author Jason R. Eads <eadsjr>
 */
public class ConnectionLifecycle implements ServiceLifecycle<Connection> {
    private final static Logger theLogger = Logger.getLogger(ConnectionLifecycle.class.getName());
    /**
     * This is the format string for Advanced Message Queuing Protocol (AMQP).
     * AMQP is a language agnostic implementation similar to Java Messaging
     * Service (JMS)
     *
     * This is used to make a connection to the QPID server.
     */
    private final static String theAMQPFormatString = "amqp://%s:%s@%s/%s?brokerlist='%s'";

    /**
     * This formats the address to properly extend the ampqURL.
     */
    private final static String theTCPAddressFormatString = "tcp://%s:%s";

    /**
     * This provides the classnames for use in JFlux.
     */
    private final static String[] theClassNameArray = new String[]{Connection.class.getName()};
    
    /**
     * Message Strings for Logger.
     */
    private final static String theFailedToCreateFactoryErrorMessage = "AMQP URL failed to create AMQConnectionFactory.";
    private final static String theFailedToProduceOrStartErrorMessage = "AMQP URL failed to produce or start an AMQconnection.";
    private final static String theFailedToStopConnectionErrorMessage = "Failed to stop AMQP connection.";
    
    /**
     * The Spec from which the object is created.
     */
    ConnectionSpec myConnectionSpec;
    
    /**
     * Lifecycle Constructor
     * 
     * @param aConnectionSpec data item to draw raw info from
     */
    public ConnectionLifecycle( ConnectionSpec aConnectionSpec) {
        myConnectionSpec = aConnectionSpec;
    }
    
    /**
     * Informs JFlux of all the dependencies this Connection object requires to
     * provide its service. In this case, none.
     * 
     * @return the dependency spec list.
     */
    @Override
    public List getDependencySpecs() {
        return Collections.EMPTY_LIST;
    }

    /**
     * Builds up the object that provides the service.
     * 
     * @param dependencyMap A map of the dependencies provided.
     * @return The actual object that provides the service function.
     */
    @Override
    public Connection createService(Map<String,Object> dependencyMap) {
        // The address extension to the url
        String address = String.format(theTCPAddressFormatString,
                myConnectionSpec.getIpAddress(),
                myConnectionSpec.getPort());

        // The URL used for QPID messaging.
        String amqpURL = String.format(theAMQPFormatString,
                myConnectionSpec.getUsername(),
                myConnectionSpec.getPassword(),
                myConnectionSpec.getClientName(),
                myConnectionSpec.getVirtualHost(),
                address);
        
        // Feed the URL into the connectionFactory 
        AMQConnectionFactory connectionFactory;
        try {
            connectionFactory = new AMQConnectionFactory(amqpURL);
        } catch (URLSyntaxException ex) {
            theLogger.log(Level.SEVERE, theFailedToCreateFactoryErrorMessage, ex);
            return null;
        }
        
        // Retrieve the connection from the factory, activate it
        Connection connection = null;
        try {
            connection = connectionFactory.createConnection();
            connection.start();
        }
        catch (JMSException ex) {
            theLogger.log(Level.SEVERE, theFailedToProduceOrStartErrorMessage, ex);
        }
        return connection;
    }

    /**
     * Ensures the object can gracefully handle a change in its dependencies.
     * The Connection has no dependencies, so is unchanged.
     * 
     * @param service The Connection object.
     * @param changeType What kind of change occurred. Defined in ServiceLifecycle.
     * @param dependencyName The name of the dependency.
     * @param dependency The dependency object.
     * @param availableDependencies Map of all available dependencies.
     * @return Returns the service.
     */
    @Override
    public Connection handleDependencyChange(Connection service, String changeType, String dependencyName, Object dependency, Map<String,Object> availableDependencies) {
        return service;
    }

    /**
     * Gracefully tears down the Connection.
     * 
     * @param service the Connection object to be disposed
     * @param availableDependencies dependencies that are available for the object.
     */
    @Override
    public void disposeService(Connection service, Map<String,Object> availableDependencies) {
        try {
            service.stop();
        } catch (JMSException ex) {
            theLogger.log(Level.WARNING, theFailedToStopConnectionErrorMessage, ex);
        }
    }

    /**
     * Returns the classname used for JFlux.
     * 
     * @return Array of the relevant class names 
     */
    @Override
    public String[] getServiceClassNames() {
        return theClassNameArray;
    }
}
