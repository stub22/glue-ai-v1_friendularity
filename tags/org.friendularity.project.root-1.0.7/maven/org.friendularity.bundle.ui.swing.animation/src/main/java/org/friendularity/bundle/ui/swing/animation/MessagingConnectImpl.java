/*
 * Copyright 2014 by The Friendularity Project (www.friendularity.org).
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

package org.friendularity.bundle.ui.swing.animation;

import java.net.URISyntaxException;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jms.Connection;
import javax.jms.ConnectionFactory;
import javax.jms.Destination;
import javax.jms.JMSException;
import javax.jms.Session;
import org.apache.qpid.client.AMQAnyDestination;
import org.apache.qpid.client.AMQConnectionFactory;
import org.jflux.api.core.Source;

/**
 *
 * @author matt
 */


public class MessagingConnectImpl {
    private final static Logger theLogger = Logger.getLogger(MessagingConnectImpl.class.getName());
    private final static String theAMQPFormatString = "amqp://%s:%s@%s/%s?brokerlist='%s'";
    Connection myConnection;
    Session mySession;
    Destination myDestination;
    
    private Source<String> myIpSource;
    private Source<String> myDestSource;
    private Source<String> myPortSource;
    private Source<String> myUsernameSource;
    private Source<String> myPasswordSource;
    private Source<String> myClientNameSource;
    private Source<String> myVirtualHostSource;
    
    public void setBrokerAddress(
            Source<String> ipSrc, 
            Source<String> portSource,
            Source<String> usernameSource,
            Source<String> passwordSource,
            Source<String> clientNameSource,
            Source<String> virtualHostSource){
        if(ipSrc == null || portSource == null
                || usernameSource == null || passwordSource == null
                || clientNameSource == null || virtualHostSource == null){
            throw new NullPointerException();
        }
        myIpSource = ipSrc;
        myPortSource = portSource;
        myUsernameSource = usernameSource;
        myPasswordSource = passwordSource;
        myClientNameSource = clientNameSource;
        myVirtualHostSource = virtualHostSource;
    }
    
    public void setDestination(Source<String> src){
        if(src == null){
            throw new NullPointerException();
        }
        myDestSource = src;
    }
    
    public boolean connect(){
        myDestination = buildDestination();
        if(myDestination == null){
            return false;
        }
        myConnection = buildConnection();
        if(myConnection == null){
            return false;
        }
        mySession = buildSession(myConnection);
        if(mySession == null){
            disconnect();
            return false;
        }
        return true;
    }
    
    public void disconnect(){
        if(mySession != null){
//            try{
//                mySession.close();
//            }catch(JMSException ex){}
            mySession = null;
        }
        if(myConnection != null){
            try{
                myConnection.close();
            }catch(JMSException ex){}
            myConnection = null;
        }
    }
    
    private String createAMQPConnectionURL(
            String username, String password, 
            String clientName, String virtualHost, String tcpAddress){
        return String.format(theAMQPFormatString, 
                username, password, clientName, virtualHost, tcpAddress);
    }
    
    private Connection buildConnection(){
        String ip = myIpSource.getValue();
        if(ip == null){
            throw new NullPointerException();
        }
        String port = myPortSource.getValue();
        String addr = "tcp://" + ip + ":" + port;
        String url = createAMQPConnectionURL(
                myUsernameSource.getValue(), 
                myPasswordSource.getValue(), 
                myClientNameSource.getValue(), 
                myVirtualHostSource.getValue(), 
                addr);
        try{ 
            String reconnectOptions = "&connectdelay='5000'&retries='2147483647'";
            url += reconnectOptions;
            ConnectionFactory fact = new AMQConnectionFactory(url);
            Connection con = fact.createConnection();
            if(con == null){
                return null;
            }
            con.start();
            return con;
        }catch(Exception ex){
            theLogger.log(Level.WARNING, "Error creating Session.", ex);
            return null;
        }
    }
    
    private Session buildSession(Connection con){
        if(con == null){
            throw new NullPointerException();
        }
        try{
            return con.createSession(false, Session.CLIENT_ACKNOWLEDGE);
        }catch(Exception ex){
            theLogger.log(Level.WARNING, "Error creating Session.", ex);
            return null;
        }
    }
    
    private Destination buildDestination(){
        String dest = myDestSource.getValue();
        if(dest == null){
            throw new NullPointerException();
        }
        try{
            return new AMQAnyDestination(dest);
        }catch(URISyntaxException ex){
            theLogger.log(Level.WARNING, "Error creating Destination.", ex);
            return null;
        }
    }
    
    public Session getSession(){
        return mySession;
    }
    
    public Destination getDestination(){
        return myDestination;
    }
}
