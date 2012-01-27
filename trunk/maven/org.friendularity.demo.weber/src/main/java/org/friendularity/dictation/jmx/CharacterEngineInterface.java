/*
 *  Copyright 2010 Hanson Robotics Inc.
 *  All Rights Reserved.
 */

package org.friendularity.dictation.jmx;

import org.friendularity.dictation.main.ContextMatchSet;
import org.friendularity.dictation.main.Match;

import java.util.Map;
import javax.management.AttributeChangeNotification;
import javax.management.JMX;
import javax.management.MBeanServerConnection;
import javax.management.Notification;
import javax.management.NotificationFilter;
import javax.management.NotificationListener;
import javax.management.ObjectName;
import javax.management.remote.JMXConnector;
import javax.management.remote.JMXConnectorFactory;
import javax.management.remote.JMXServiceURL;
import org.cogchar.integroid.jmxwrap.IntegroidWrapperMXBean;
import org.cogchar.api.platform.cues.ThoughtCue;
import org.cogchar.platform.stub.CueStub;

/**
 *
 * @author Matt Stevenson
 */
public class CharacterEngineInterface implements NotificationListener {
    private static IntegroidWrapperMXBean myIntegroidProxy;
	private ObjectName myIntegroidON;
	private MBeanServerConnection myMBSC;
    private JMXConnector jmxc;
    private String myURL;

	public void connect(String serviceUrl){
        myURL = serviceUrl;
		try{
			JMXServiceURL jurl = new JMXServiceURL(serviceUrl);
			jmxc = JMXConnectorFactory.connect(jurl, null);

			myMBSC = jmxc.getMBeanServerConnection();
			myIntegroidON = new ObjectName(IntegroidWrapperMXBean.INTEGROID_JMX_OBJNAME);
			myIntegroidProxy = JMX.newMXBeanProxy(myMBSC, myIntegroidON, IntegroidWrapperMXBean.class);
            registerHandlers();

		}catch(Throwable t){
            t.printStackTrace();
        }
	}
	private void registerHandlers() throws Throwable {
		NotificationFilter filter = null;
		Object handback = null;
		myMBSC.addNotificationListener(myIntegroidON, this, filter, handback);
	}

	public void sendResponseMeaningToConvoid(final Map<String,Double> meanings, double c) {
		final double conf = c;
		new Thread(new Runnable(){
			public void run() {
                try{
                    myIntegroidProxy.postVerbalCue(meanings, conf);
                }catch(Throwable t){
                    t.printStackTrace();
                }
			}
		}).start();
	}

	public void sendInformationToConvoid(String n, String m){
		final String name = n;
		final String meaning = m;
		new Thread(new Runnable(){
			public void run() {
				try{
					myIntegroidProxy.postTextCue(name, meaning, 1.0);
				}catch(Throwable t){
					t.printStackTrace();
				}
			}
		}).start();
	}

	public void sendHeardCue(String t){
		final String text = t;
		new Thread(new Runnable(){
			public void run() {
				try{
					myIntegroidProxy.postHeardCue(text);
				}catch(Throwable t){
					t.printStackTrace();
				}
			}
		}).start();
	}

    public void handleNotification(Notification notification, Object handback) {
		try {
			if (notification instanceof AttributeChangeNotification) {
				AttributeChangeNotification acn = (AttributeChangeNotification) notification;
				String attribName = acn.getAttributeName();
				Object newValue =  acn.getNewValue();

				if (attribName.equals(IntegroidWrapperMXBean.ATTRIB_CUE_POSTED)) {
					CueStub cue = (CueStub) newValue;
					handlePostedCue(cue);
				} else 	if (attribName.equals(IntegroidWrapperMXBean.ATTRIB_CUE_CLEARED)) {
					CueStub cue = (CueStub) newValue;
                    handleClearedCue(cue);
				}
			}
		} catch (Throwable t){
            t.printStackTrace();
        }
	}
	private void handlePostedCue(CueStub c) throws Throwable  {
        if(c.getClass() == ThoughtCue.class){
            ThoughtCue tc = (ThoughtCue)c;
            if(tc.getName().startsWith("T_EXPECT-")){
                long start = tc.getCreateStampMsec();
                System.out.println("Cue Created at: " + tc.getCreateStampMsec());
                long now = System.currentTimeMillis();
                System.out.println("Received: " + tc.getName() + " at " + now);
                long elapsed = now - start;
                System.out.println("Time to post: " + elapsed);
                String name = tc.getName().replace("T_EXPECT", "ANS");
                Match m = new Match(name, name, 100.0);
                //ContextMatchSet.clear();
                ContextMatchSet.addMatch(m);
            }
        }
	}
	private void handleClearedCue(CueStub c) throws Throwable  {
        if(c.getClass() == ThoughtCue.class){
            ThoughtCue tc = (ThoughtCue)c;
            if(tc.getName().startsWith("T_EXPECT-")){
                System.out.println("Cleared: " + tc.getName() + " at " + System.currentTimeMillis());
                String name = tc.getName().replace("T_EXPECT", "ANS");
                ContextMatchSet.removeMeaning(name);
            }
        }
	}
}
