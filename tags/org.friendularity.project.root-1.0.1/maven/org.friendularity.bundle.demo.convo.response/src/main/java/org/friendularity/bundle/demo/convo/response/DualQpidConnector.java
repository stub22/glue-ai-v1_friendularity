/*
 * Copyright 2013 Hanson Robokind LLC.
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
package org.friendularity.bundle.demo.convo.response;

import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jms.Connection;
import javax.jms.Destination;
import javax.jms.JMSException;
import javax.jms.MessageConsumer;
import javax.jms.Session;
import org.apache.avro.Schema;
import org.jflux.api.core.util.DefaultNotifier;
import org.robokind.api.messaging.RecordAsyncReceiver.RecordHandler;
import org.robokind.impl.messaging.JMSAvroRecordAsyncReceiver;
import org.robokind.impl.messaging.JMSAvroRecordSender;
import org.robokind.impl.messaging.JMSBytesMessageSender;
import org.robokind.impl.messaging.utils.ConnectionManager;
import org.robokind.impl.speech.SpeechRequestRecord;
import org.robokind.impl.speechrec.SpeechRecEventListRecord;

/**
 *
 * @author Jason G. Pallack <jgpallack@gmail.com>
 */
public class DualQpidConnector extends DefaultNotifier<SpeechRecEventListRecord> {
    private final static Logger theLogger =
            Logger.getLogger(DualQpidConnector.class.getName());
    private String myIPAddress;
    private String myDestinationString;
    private String myOutDestinationString;
    
    private Connection myConnection;
    private Session mySession;
    private Destination myDestination;
    private Destination myOutDestination;
    private MessageConsumer myConsumer;
    private JMSAvroRecordAsyncReceiver<SpeechRecEventListRecord> myReceiver;
    private JMSAvroRecordSender<SpeechRequestRecord> mySender;
    private JMSBytesMessageSender myBytesSender;
    
    private Schema mySchema;
    private Schema myOutSchema;
    private RecordNotifier myNotifier;
    
    public void connect(){
        //IP Address needs port number, the default port is 5672
        try {
            myConnection = ConnectionManager.createConnection(
                    "admin", "admin", "client1", "test",
                    "tcp://" + myIPAddress + ":5672");
            myDestination =
                    ConnectionManager.createDestination(myDestinationString);
            myOutDestination =
                    ConnectionManager.createDestination(myOutDestinationString);
            try{
                mySession =
                        myConnection.createSession(
                        false, Session.CLIENT_ACKNOWLEDGE);
                myConsumer = mySession.createConsumer(myDestination);
                myConnection.start();
            }catch(JMSException ex){
                theLogger.warning("Unable to create Session or Consumer");
                return;
            }
            myNotifier = new RecordNotifier();
            
            myReceiver =
                    new JMSAvroRecordAsyncReceiver<SpeechRecEventListRecord>(
                    SpeechRecEventListRecord.class, mySchema, myConsumer);
            myReceiver.setRecordHandler(myNotifier);
            myReceiver.start();
            
            myBytesSender = new JMSBytesMessageSender();
            myBytesSender.setSession(mySession);
            myBytesSender.setDestination(myOutDestination);
            myBytesSender.openProducer();
            mySender =
                    new JMSAvroRecordSender<SpeechRequestRecord>(myBytesSender);
        } catch(Exception e) {
            theLogger.log(Level.SEVERE, "Connection error: {0}", e.getMessage());
            
            disconnect();
        }
    }
    
    public void disconnect(){
        if(myReceiver != null) {
            try {
                myReceiver.stop();
            } catch(Exception ex) {
            }
        }

        if(myConsumer != null) {
            try {
                myConsumer.close();
            } catch(JMSException ex) {
            }
        }
        
        if(myBytesSender != null) {
            myBytesSender.closeProducer();
        }

        if(mySession != null) {
            try {
                mySession.close();
            } catch(JMSException ex) {
            }
        }

        if(myConnection != null) {
            try {
                myConnection.close();
            } catch(JMSException ex) {
            }
        }

        myConnection = null;
        myDestination = null;
        mySession = null;
        myConsumer = null;
        myNotifier = null;
        myReceiver = null;
        myBytesSender = null;
        mySender = null;
    }
    
    private boolean validateIP(String address) {
        String[] dotQuad = address.trim().split("\\.");
        
        if(dotQuad.length != 4) { // IP is four segments separated by .s
            return false;
        }
        
        for(String segment: dotQuad) {
            try {
                Integer segInt = Integer.parseInt(segment);
                
                if(segInt < 0 || segInt > 255) { // each segment is 0-255
                    return false;
                }
            } catch(Exception e) { // each segment must be a valid int
                return false;
            }
        }
        
        return true;
    }
    
    public void setIPAddress(String ipAddress) {
        if(validateIP(ipAddress)) {
            myIPAddress = ipAddress;
        } else {
            throw new IllegalArgumentException("Invalid IP address" + ipAddress);
        }
    }
    
    public void setDestinationString(String destinationString) {
        myDestinationString = destinationString;
    }
    
    public void setOutDestinationString(String destinationString) {
        myOutDestinationString = destinationString;
    }
    
    public void setSchema(Schema schema) {
        mySchema = schema;
    }
    
    public void setOutSchema(Schema schema) {
        myOutSchema = schema;
    }
    
    public Session getSession() {
        return mySession;
    }
    
    public Destination getDestination() {
        return myDestination;
    }
    
    public Destination getOutDestination() {
        return myOutDestination;
    }
    
    class RecordNotifier implements RecordHandler<SpeechRecEventListRecord>{
        @Override
        public void handleRecord(SpeechRecEventListRecord t) {
            if(t != null){
                notifyListeners(t);
            }
        }
    }
    
    public void send(SpeechRequestRecord record) {
        mySender.sendRecord(record);
    }
}
