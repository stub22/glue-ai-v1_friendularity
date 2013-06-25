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
package org.friendularity.demo.connection;

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
import org.jflux.api.service.DependencySpec;

/**
 * This lifecycle comprises the JFlux object registry interface for the
 * Connection object.
 * 
 * @author Jason R. Eads <eadsjr>
 */
public class ConnectionLifecycle implements ServiceLifecycle<Connection> {
    
    /**
     * This is the format string for AMQP: Advanced Message Queuing Protocol.
     * AMQP is a language agnostic implementation similar to JMS: Java Messaging
     * Service
     *
     * This is used to make a connection to the QPID server.
     */
    private final static String theAMQPFormatString = "amqp://%s:%s@%s/%s?brokerlist='%s'";

    /**
     * TODO: clarify description.
     *
     * This formats the address to properly extend the ampqURL.
     */
    private final static String theTCPAddressFormatString = "tcp://%s:%s";
    
    ConnectionSpec myConnectionSpec;
    
    public ConnectionLifecycle( ConnectionSpec aConnectionSpec) {
        myConnectionSpec = aConnectionSpec;
    }
    
    @Override
    public List getDependencySpecs() {
        return Collections.EMPTY_LIST;
    }

    @Override
    public Connection createService(Map<String,Object> dependencyMap) {
        
        //<editor-fold defaultstate="collapsed" desc=" Build the URL from the Spec ">

        // The address extension to the url
        String Address = String.format(theTCPAddressFormatString,
                myConnectionSpec.getIpAddress(),
                myConnectionSpec.getPort());

        // The URL used for QPID messaging.
        String amqpURL = String.format(theAMQPFormatString,
                myConnectionSpec.getUsername(),
                myConnectionSpec.getPassword(),
                myConnectionSpec.getClientName(),
                myConnectionSpec.getVirtualHost(),
                Address);
        //</editor-fold>
        
        //<editor-fold defaultstate="collapsed" desc=" Use the URL to make the connection. ">

        // Feed the URL into the connectionFactory 
        AMQConnectionFactory connectionFactory = null;
        try {
            connectionFactory = new AMQConnectionFactory(amqpURL);
            } catch (URLSyntaxException ex) {
            Logger.getLogger(ConnectionLifecycle.class.getName()).log(Level.SEVERE, "AMQP URL failed to create AMQConnectionFactory.", ex);
        }
        
        // Retrieve the connection from the factory, activate it
        Connection connection = null;
        try {
            if( connectionFactory != null ) {
                connection = connectionFactory.createConnection();
                connection.start();
            }
        }
        catch (JMSException ex) {
            Logger.getLogger(ConnectionLifecycle.class.getName()).log(Level.SEVERE, "AMQP URL failed to produce or start an AMQconnection.", ex);
        }
                //</editor-fold>

        return connection;
    }

    @Override
    public Connection handleDependencyChange(Connection service, String changeType, String dependencyName, Object dependency, Map<String,Object> availableDependencies) {
        return service;
    }

    @Override
    public void disposeService(Connection service, Map<String,Object> availableDependencies) {
        try {
            service.stop();
        } catch (JMSException ex) {
            Logger.getLogger(ConnectionLifecycle.class.getName()).log(Level.SEVERE, "Failed to stop AMQP connection.", ex);
        }
    }

    private final static String[] theClassNameArray = new String[]{Connection.class.getName()};
    
    @Override
    public String[] getServiceClassNames() {
        return theClassNameArray;
    }
}
