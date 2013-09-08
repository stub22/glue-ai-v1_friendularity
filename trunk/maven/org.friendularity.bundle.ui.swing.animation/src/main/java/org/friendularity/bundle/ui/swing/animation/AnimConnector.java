/*
 * Copyright 2012 Hanson Robokind LLC.
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

import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jms.Connection;
import javax.jms.JMSException;
import org.jflux.api.core.Source;
import org.osgi.framework.BundleContext;
import org.robokind.api.animation.Animation;
import org.robokind.api.animation.lifecycle.AnimationPlayerClientLifecycle;
import org.robokind.api.animation.messaging.RemoteAnimationPlayerClient;
import org.robokind.api.animation.protocol.AnimationEvent;
import org.robokind.api.animation.protocol.AnimationEvent.AnimationEventFactory;
import org.robokind.api.animation.protocol.AnimationSignal;
import org.robokind.api.common.lifecycle.ManagedService;
import org.robokind.api.common.lifecycle.utils.SimpleLifecycle;
import org.robokind.api.common.osgi.OSGiUtils;
import org.robokind.api.common.osgi.lifecycle.OSGiComponent;
import org.robokind.impl.animation.messaging.AnimationRecord;
import org.robokind.impl.animation.messaging.AnimationSignallingRecord;
import org.robokind.impl.animation.messaging.PortableAnimationEvent;
import org.robokind.impl.animation.messaging.PortableAnimationSignal;
import org.robokind.impl.messaging.lifecycle.JMSAvroAsyncReceiverLifecycle;
import org.robokind.impl.messaging.lifecycle.JMSAvroMessageSenderLifecycle;
import org.robokind.impl.messaging.utils.ConnectionManager;
import org.robokind.impl.messaging.utils.ConnectionUtils;

/**
 *
 * @author matt
 */
public class AnimConnector implements Source<RemoteAnimationPlayerClient>{
    private final static Logger theLogger = Logger.getLogger(AnimConnector.class.getName());
    private AnimationPlayerClientLifecycle myLifecycle;
    private ManagedService myPlayerService;
    private ManagedService mySenderService;
    private ManagedService myConnectionService;
    private ManagedService mySessionService;
    private ManagedService myReceiverService;
    private JMSAvroMessageSenderLifecycle mySenderLife;
    private Connection myConnection;
    private String myAnimDestString;
    private int myAnimDestType;
    private boolean myStartFlag;
    private Source<String> myIPSource;
    
    public AnimConnector(Source<String> ipSource, 
            String animDestStr, Integer animDestType){
        myIPSource = ipSource;
        myStartFlag = false;
        myAnimDestString = animDestStr != null ? animDestStr : "animationRequest";
        myAnimDestType = animDestType != null ? animDestType : ConnectionUtils.QUEUE;
    }
    
    public synchronized boolean connect(){
        if(myStartFlag){
            return true;
        }
        BundleContext context = OSGiUtils.getBundleContext(Animation.class);
        String ip = myIPSource.getValue();
        myConnection = ConnectionManager.createConnection(
                "admin", "admin", "client1", "test", 
                "tcp://" + ip + ":5672");
        try{
            myConnection.start();
        }catch(JMSException ex){
            theLogger.log(Level.WARNING, "Unable to connect to {0}", ip);
            return false;
        }
        myConnectionService = new OSGiComponent(context, new SimpleLifecycle(myConnection, Connection.class));
        myConnectionService.start();
        ConnectionUtils.ensureSession(context, 
                "remoteAnimConnection", myConnection, null);
        ConnectionUtils.ensureDestinations(context, 
                "remoteAnimationRequest", myAnimDestString, myAnimDestType, null);
        mySenderLife = new JMSAvroMessageSenderLifecycle(
                        new PortableAnimationEvent.MessageRecordAdapter(), 
                        AnimationEvent.class, AnimationRecord.class, 
                        "remoteAnimSender", "remoteAnimConnection", 
                        "remoteAnimationRequest");
        registerEventFactory(context);
        mySenderService = new OSGiComponent(context, mySenderLife);
        mySenderService.start();
        ConnectionUtils.ensureSession(context, 
                "remoteSignalConnection", myConnection, null);
        ConnectionUtils.ensureDestinations(context, 
                "remoteAnimationSignal", "animationSignal", ConnectionUtils.TOPIC, null);
        JMSAvroAsyncReceiverLifecycle receiverLife =
                new JMSAvroAsyncReceiverLifecycle(
                new PortableAnimationSignal.RecordMessageAdapter(),
                AnimationSignal.class, AnimationSignallingRecord.class,
                AnimationSignallingRecord.SCHEMA$, "remoteSignalReceiver",
                "remoteSignalConnection", "remoteAnimationSignal");
        myReceiverService = new OSGiComponent(context, receiverLife);
        myReceiverService.start();
        myLifecycle = 
                new AnimationPlayerClientLifecycle(
                "remotePlayer", "remotePlayer", "remoteAnimSender",
                "remoteSignalReceiver", context);
        myPlayerService = new OSGiComponent(context, myLifecycle);
        myPlayerService.start();
        myStartFlag = true;
        return true;
    }
    
    private void registerEventFactory(BundleContext context){
        if(OSGiUtils.serviceExists(context, AnimationEvent.AnimationEventFactory.class, null)){
            return;
        }
        new OSGiComponent(context, 
                new SimpleLifecycle(
                        new PortableAnimationEvent.Factory(), 
                        AnimationEventFactory.class)
                ).start();
    }
    
    public synchronized void stop(){
        if(myPlayerService != null){
            myPlayerService.dispose();
            myPlayerService = null;
        }
        if(mySenderService != null){
            mySenderService.dispose();
            mySenderService = null;
        }
        if(myConnectionService != null){
            myConnectionService.dispose();
            myConnectionService = null;
        }
        if(mySessionService != null){
            mySessionService.dispose();
            mySessionService = null;
        }
        try{
            myConnection.close();
        }catch(JMSException ex){
        }
        myStartFlag = false;
    }
    
    
    @Override
    public RemoteAnimationPlayerClient getValue() {
        if(myLifecycle == null){
            return null;
        }
        return myLifecycle.getService();
    }
}