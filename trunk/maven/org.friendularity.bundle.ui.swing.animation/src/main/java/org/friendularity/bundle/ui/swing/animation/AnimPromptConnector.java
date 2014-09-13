/*
 * Copyright 2014 the Friendularity Project
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

import java.util.Arrays;
import java.util.List;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.jms.Destination;
import javax.jms.JMSException;
import javax.jms.Session;
import org.jflux.api.common.rk.config.VersionProperty;
import org.jflux.api.core.Listener;
import org.jflux.api.core.Source;
import org.jflux.api.core.config.ConfigProperty;
import org.jflux.api.core.config.DefaultConfigProperty;
import org.jflux.api.core.node.ProducerNode;
import org.jflux.api.core.util.DefaultSource;
import org.jflux.api.core.util.EmptyAdapter;
import org.jflux.api.messaging.rk.services.ServiceCommand;
import org.jflux.impl.messaging.JMSAvroUtils;
import org.jflux.impl.messaging.rk.ServiceCommandRecord;
import org.mechio.api.animation.Animation;
import org.mechio.api.animation.library.AnimationLibrary;
import org.mechio.api.animation.library.DefaultAnimationLibrary;
import org.mechio.api.animation.messaging.RemoteAnimationPlayerClient;

/**
 *
 * @author matt
 */
public class AnimPromptConnector implements Source<AnimationLibrary>{
    private final static Logger theLogger = Logger.getLogger(AnimPromptConnector.class.getName());
    private AnimationLibrary myLibrary;
    private Source<RemoteAnimationPlayerClient> myPlayerSource;
    private ProducerNode<ServiceCommand> myPromptProducer;
    private Listener<String> myIpListener;
    
    private MessagingConnectImpl myPromptConnect;
    
    public AnimPromptConnector() {
        ConfigProperty<String> destProp = 
                new DefaultConfigProperty<String>(String.class, 
                        "animPrompt; {create: always, node: {type: topic}}");
        
        myPromptConnect = new MessagingConnectImpl();
        myPromptConnect.setDestination(destProp.getSource());
        ConfigProperty<String> ipAddrProp = 
                new DefaultConfigProperty<String>(String.class, 
                        "127.0.0.1");
        myIpListener = ipAddrProp.getSetter();
        myPromptConnect.setBrokerAddress(ipAddrProp.getSource(), 
                new DefaultSource<String>("5672"), 
                new DefaultSource<String>("admin"), 
                new DefaultSource<String>("admin"), 
                new DefaultSource<String>("client1"), 
                new DefaultSource<String>("test"));
        myLibrary = new DefaultAnimationLibrary("prompt");
    }
    
    public Listener<String> getIpSetter(){
        return myIpListener;
    }
    
    private final static String SLEEP_DELIM = ":SLEEP:";
    public boolean connect(){
        if(!myPromptConnect.connect()){
            return false;
        }
        Session session = myPromptConnect.getSession();
        Destination dest = myPromptConnect.getDestination();
        myPromptProducer = buildSpeechRecChain(session, dest);
        myPromptProducer.getNotifier().addListener(
                new Listener<ServiceCommand>() {
                    @Override
                    public void handleEvent(ServiceCommand input) {
                        String animName = input.getCommand();
                        if(animName == null){
                            return;
                        }
                        int sleepIndex = animName.indexOf(SLEEP_DELIM);
                        if(sleepIndex == -1){
                            playAnim(animName);
                        }else{
                            final String realName = animName.substring(0, sleepIndex);
                            String sleepTimeStr = animName.substring(sleepIndex+SLEEP_DELIM.length());
                            final long sleepTime = Long.parseLong(sleepTimeStr);
                            ScheduledExecutorService s = new ScheduledThreadPoolExecutor(1);
                            s.schedule(
                                    new Runnable() {
                                        @Override
                                        public void run() {
                                            playAnim(realName);
                                        }
                                    }, 
                                    sleepTime, TimeUnit.MILLISECONDS);
                        }
                    }
                });
        myPromptProducer.start();
        return true;
    }
    
    private void playAnim(String animName){
        RemoteAnimationPlayerClient player = myPlayerSource.getValue();
        if(player == null){
            theLogger.info("No player set, unable to play: " + animName);
            return;
        }
        theLogger.info("Received Anim Prompt: " + animName);
        VersionProperty p = new VersionProperty(animName, "1.0");
        Animation anim = myLibrary.getAnimation(p);
        if(anim == null){
            animName = animName.toLowerCase();
            p = new VersionProperty(animName, "1.0");
            anim = myLibrary.getAnimation(p);
        }
        if(anim == null){
            animName = animName.toUpperCase();
            p = new VersionProperty(animName, "1.0");
            anim = myLibrary.getAnimation(p);
        }
        if(anim == null){
            theLogger.info("No anim found for: " + animName);
            restartDefs();
            return;
        }
        if(myPlayerSource == null){
            theLogger.info("No player set, unable to play: " + animName);
            return;
        }
        theLogger.info("Playing animation: " + animName);
        if(anim.getChannels().size() > 10){
            stop("FACE");
            stop("LEGS");
        }
        stop("ARMS");
        player.playAnimation(anim);
        waitRestart(anim.getLength() + 1500);
    }
    
    public void stop(){
        myPromptProducer.stop();
        myPromptConnect.disconnect();
    }
    

    public void setPlayerSource(Source<RemoteAnimationPlayerClient> playerSource){
        myPlayerSource = playerSource;
    }
    
    private void waitRestart(final long time){
        new Thread(new Runnable() {
            @Override
            public void run() {
                try{
                    Thread.sleep(time);
                }catch(Exception ex){}
                restartDefs();
            }
        }).start();
    }
    
    private ProducerNode<ServiceCommand> buildSpeechRecChain(
            Session session, Destination dest){
        try{
            return JMSAvroUtils.buildEventReceiverChain(
                    ServiceCommandRecord.class, 
                    ServiceCommandRecord.SCHEMA$, 
                    new EmptyAdapter(), 
                    session, dest);
        }catch(JMSException ex){
            theLogger.log(Level.WARNING,"Error connecting to Anim Prompt.",ex);
            return null;
        }
    }
    
    @Override
    public AnimationLibrary getValue() {
        return myLibrary;
    }
    private List<String> defAnims = Arrays.asList("LEGS","BLINK","FACE","ARMS");
    
    private void startDefs(){
        for(String s : defAnims){
            loop(s);
        }
    }
    
    private void restartDefs(){
        RemoteAnimationPlayerClient p = myPlayerSource.getValue();
        if(myLibrary == null || p == null){
            return;
        }
        for(VersionProperty prop : myLibrary.getAnimationVersions()){
            if(!defAnims.contains(prop.getName())){
                Animation a = myLibrary.getAnimation(prop);
                p.stopAnimation(a);
            }
        }
        startDefs();
    }
    
    private void loop(String anim){
        RemoteAnimationPlayerClient p = myPlayerSource.getValue();
        if(myLibrary == null || p == null){
            return;
        }
        Animation legs = myLibrary.getAnimation(new VersionProperty(anim, "1.0"));
        if(legs != null){
            p.loopAnimation(legs);
        }
    }
    
    private void stop(String anim){
        RemoteAnimationPlayerClient p = myPlayerSource.getValue();
        if(myLibrary == null || p == null){
            return;
        }
        Animation legs = myLibrary.getAnimation(new VersionProperty(anim, "1.0"));
        if(legs != null){
            p.stopAnimation(legs);
        }
    }
}
