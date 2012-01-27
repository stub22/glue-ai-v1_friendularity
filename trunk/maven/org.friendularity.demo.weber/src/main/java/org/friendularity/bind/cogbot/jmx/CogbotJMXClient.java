/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.friendularity.bind.cogbot.jmx;

/**
 *
 *  Copyright 2010 Hanson Robotics Inc.
 *  All Rights Reserved.
 */
import org.friendularity.bind.cogbot.scripting.SerialEventQueue;


import org.cogchar.integroid.jmxwrap.IntegroidWrapperMXBean;
import org.cogchar.integroid.jmxwrap.JMXUtils;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.HashMap;
import javax.management.ObjectName;
import javax.management.remote.JMXServiceURL;

import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.management.AttributeChangeNotification;
import javax.management.InstanceNotFoundException;
import javax.management.JMX;
import javax.management.MBeanServerConnection;
import javax.management.Notification;
import javax.management.NotificationFilter;
import javax.management.NotificationListener;
import javax.management.remote.JMXConnector;
import javax.management.remote.JMXConnectorFactory;
import org.friendularity.bind.cogbot.service.CogbotService;
import org.cogchar.animoid.job.AnimationExecJob;
import org.cogchar.animoid.job.VisemeJob;
import org.cogchar.api.animoid.protocol.Animation;
import org.cogchar.platform.stub.CueStub;
import org.cogchar.platform.stub.JobStub;

/**
 * @author Stu Baurmann/Douglas Miles
 */
public class CogbotJMXClient implements NotificationListener, NotificationFilter {
	private static Logger	theLogger = Logger.getLogger(CogbotJMXClient.class.getName());

    public static String serviceURL = "service:jmx:rmi:///jndi/rmi://localhost:7227/jmxrmi";
    transient private MBeanServerConnection myMBSC;
    //volatile MBeanServerInvocationHandler handler;
    transient private ObjectName myIntegroidON;
    transient private IntegroidWrapperMXBean myIntegroidProxy;
    transient PrintWriter debugPw;
    boolean jmxServiceReady = false;
    private boolean isEnabled = true;
    final private SerialEventQueue OneAtATime;
    HashMap<NotificationListener, SerialEventQueue> listeners = new HashMap<NotificationListener, SerialEventQueue>();

    public CogbotJMXClient(String serviceURL0, PrintWriter pw) throws Throwable {
        OneAtATime = new SerialEventQueue("CogbotJMXClient OneAtATime " + serviceURL0);
        debugPw = pw != null ? pw : new PrintWriter(System.err);
        new Thread(new Runnable() {

            public void run() {
                try {
                    connectJMX(serviceURL);
                } catch (Throwable ex) {
                    echo("connectJMX: ", ex);
                    Logger.getLogger(CogbotJMXClient.class.getName()).log(Level.SEVERE, null, ex);
                }
            }
        }).start();
    }

    public void registerListener(NotificationListener listener) {
        synchronized (listeners) {
            if (listeners.containsKey(listener)) {
                return;
            }
            listeners.put(listener, new SerialEventQueue("Cogbot JMX Client"));
        }
    }

    public void deregisterListener(NotificationListener listener) {
        synchronized (listeners) {
            SerialEventQueue que = listeners.remove(listener);
            if (que!=null) que.stop();
        }
    }

    public void connectJMX(String serviceURL) throws IOException {
        if (CogbotService.disableJMX) {
            throw new IOException("Attempt to connect to JMX while CogbotService.disableJMX == true");
        }
        JMXServiceURL url = new JMXServiceURL(serviceURL);
        JMXConnector jmxc = JMXConnectorFactory.connect(url, null);

        // Get an MBeanServerConnection
        echo("\nFetching an MBeanServerConnection");
        myMBSC = jmxc.getMBeanServerConnection();

        try {
            JMXUtils.dumpMBeanServerInfo(myMBSC);
        } catch (Throwable e) {
            echo("dumpMBeanServerInfo: ", e);
        }

        // If we setup handlers before sending our messages, it slows down the
        // computer a lot, and the two text cues get spoken in reverse order.
        try {
            connectProxies();
        } catch (Throwable e) {
            e.printStackTrace();
            echo("connectProxies: ", e);
        }

        try {
            registerHandlers();
        } catch (Throwable e) {
            e.printStackTrace();
            echo("registerHandlers: ", e);
        }
    }

    private void connectProxies() throws Throwable {
        myIntegroidON = new ObjectName(IntegroidWrapperMXBean.INTEGROID_JMX_OBJNAME);
        myIntegroidProxy = JMX.newMXBeanProxy(myMBSC, myIntegroidON, IntegroidWrapperMXBean.class);
    }

    private void registerHandlers() throws Throwable {
        NotificationFilter filter = this;
        Object handback = null;
        // In this test client, all notifications are routed to the handleNotification method.
        while (true) {
            try {
                Thread.sleep(3000);
                myMBSC.addNotificationListener(myIntegroidON, this, this, handback);
                echo("addNotificationListener: SUCCESS");
                jmxServiceReady = true;
                return;
            } catch (InstanceNotFoundException e) {
                // too early
            } catch (Exception e) {
                echo("addNotificationListener: " + e);
                e.printStackTrace();
            }
        }
    }

    private void sayText(String textToSay) {
        myIntegroidProxy.postTextCue("TXT_SAY", textToSay, 1.0);
    }

    private void hearMeanings(Map<String, Double> meanings) {
        myIntegroidProxy.postVerbalCue(meanings, 1.0);
    }

    private void thinkThought(String thoughtName) {
        myIntegroidProxy.postThoughtCue(thoughtName, 1.0);
    }

    private void setVariable(String name, String val) {
        myIntegroidProxy.postVariableCue(name, val, 1.0);
    }

    private void testIntegroidProxyMethods() throws Throwable {
        Map<String, Double> meanings = new HashMap();
        meanings.put("PUZZLING", 1.0);
        meanings.put("EVIDENCE", 1.0);
        meanings.put("EINSTEIN", 1.0);

        echo("Posting thought cue");
        thinkThought("T_SASSY");
        echo("Posting verbal cue");
        hearMeanings(meanings);
        echo("Posting variable cue to VAR_LOOK_AT");
        setVariable("VAR_LOOK_AT", "shiny object");
        echo("Posting two text cues");
        String firstTextBlock =
                "<sapi>Hey, this text was sent over JMX!  Here is a bookmark for a thought "
                + " cue: <bookmark mark=\"thought:T_SILLY\"/>. </sapi>";
        String secondTextBlock = "<sapi>Now we're in another JMX-posted text cue, and here is "
                + " a bookmark for a direct animation trigger: <bookmark mark=\"anim:amused1\"/> "
                + "In other news, the value of the partner variable is ${PARTNER}, and "
                + "the value of the look at  variable is ${VAR_LOOK_AT}.  Okay, bye for "
                + "now!</sapi>";

        sayText(firstTextBlock);
        // If we don't leave a bit of a gap, sometimes the second cue gets processed first.
        Thread.sleep(1000);
        sayText(secondTextBlock);

        echo("Posting new value for VAR_LOOK_AT - will it be posted before the second speech job is configured?");
        setVariable("VAR_LOOK_AT", "blinky lights");
    }
    // It appears that this method is called in a single-threaded handler.
    // Haven't looked to see what tricks the MBSC is capable of.  Probably many.

    public void handleNotification(Notification notification0, Object handbac0) {
        final Notification notification = notification0;
        final Object handback = handbac0;
        try {
            if (CogbotService.disableJMX) {
                return;
            }
            Runnable r = new Runnable() {

                final long initMs = System.currentTimeMillis();

                public void run() {
                    echo(">>>>>>>>>>>>>>>**********************");
                    echo("Received notification at: " + initMs + " (" + (System.currentTimeMillis() - initMs) + "ms ago) ");
                    //			echo("\tClassName: " + notification.getClass().getName());
                    //			echo("\tSource: " + notification.getSource());
                    // 			echo("\tType: " + notification.getType());
                    echo("\tMessage: " + notification.getMessage());
                    echo("<<<<<<<<<<<<<<<<<<<<<<**********************");
                    synchronized (listeners) {
                        for (Map.Entry<NotificationListener, SerialEventQueue> l0 : listeners.entrySet()) {
                            final NotificationListener l = l0.getKey();
                            Runnable r = new Runnable() {

                                public void run() {
                                    l.handleNotification(notification, handback);
                                }
                            };
                            l0.getValue().invokeLater(r);
                        }
                    }
                }
            };

            OneAtATime.invokeLater(r);

            if (true) {
                return;
            }
            if (notification instanceof AttributeChangeNotification) {
                AttributeChangeNotification acn = (AttributeChangeNotification) notification;
                String attribName = acn.getAttributeName();
                String attribTypeName = acn.getAttributeType();
                Object newValue = acn.getNewValue();
                Object oldValue = acn.getOldValue();

                // Dispatch based on "attribute name"
                if (attribName.equals(IntegroidWrapperMXBean.ATTRIB_CUE_POSTED)) {
                    CueStub cue = (CueStub) newValue;
                    handlePostedCue(cue);
                } else if (attribName.equals(IntegroidWrapperMXBean.ATTRIB_CUE_CLEARED)) {
                    CueStub cue = (CueStub) newValue;
                    echo("Ignoring cueCleared notification");
                } else if (attribName.equals(IntegroidWrapperMXBean.ATTRIB_JOB_POSTED)) {
                    if (newValue instanceof JobStub) {
                        JobStub job = (JobStub) newValue;
                        handlePostedJob(job);
                    } else {
                        handlePostedJobObject(newValue);
                    }
                } else if (attribName.equals(IntegroidWrapperMXBean.ATTRIB_JOB_CLEARED)) {
                    //  Job job = (Job) newValue;
                    echo("Ignoring jobCleared notification");
                } else {
                    echo("####################################################################");
                    echo("Unhandled attribute change notification.  Details are:");
                    echo("\tAttributeName: " + attribName);
                    echo("\tAttributeType: " + attribTypeName);
                    echo("\tNewValue: " + newValue);
                    echo("\tOldValue: " + oldValue);
                    echo("####################################################################");
                }
            }
            echo("\nFinished processing notification at:" + System.currentTimeMillis());
            echo("******************");
        } catch (Throwable t) {
            t.printStackTrace();
        }
    }

    private void handlePostedJobObject(Object newValue) {
        if (true) {
            //TODO not this
            echo("IGNORING handlePostedJobObject: ", newValue);
            return;
        }
    }

    private void handlePostedCue(CueStub c) throws Throwable {
        if (true) {
            //TODO not this
            echo("IGNORING CUE: ", c);
            return;
        }
        echo("HANDLING POSTED CUE: ", c);
        // MPL TODO:  Add some dispatch logic here based on the cues you want to handle.
    }

    private void handlePostedJob(JobStub j) throws Throwable {
        if (true) {
            //TODO not this
            echo("IGNORING JOB: ", j);
        }
        // Do a crude dispatch based on job class.
        if (j instanceof VisemeJob) {
            VisemeJob vj = (VisemeJob) j;
            handleVisemeJob(vj);
        } else if (j instanceof AnimationExecJob) {
            AnimationExecJob aej = (AnimationExecJob) j;
            handleAnimationExecJob(aej);
        } else {
            echo("IGNORING UNINTERESTING JOB: ", j);
        }
    }

    private void handleVisemeJob(VisemeJob vj) throws Throwable {
        if (true) {
            //TODO not this
            echo("IGNORING VISEME: ", vj);
            return;
        }
        /*  These events arrive frequently when the robot is speaking, and printing
        all this text SOMETIMES slows down the computer.

        echo ("HANDLING POSTED VISEME JOB: " + vj);
        long durationMillisec = vj.getDurationMillisec();
        // Unless this is first VisemeJob in a stream, we should already be near these
        // "currentFrame" positions as a result of previous VisemeJob.
        Frame currentFrame = vj.getCurrentFrame();
        // nextFrame contains the positions we want to start moving towards, arriving there in approximately
        // durationMillisec.
        Frame nextFrame = vj.getNextFrame();
        // Show all the joints used by this animation.
        // echo("Used joints in the 'nextFrame' of this Viseme are: " + nextFrame.getUsedJointSet());
        if (nextFrame != null) {
        echo("viseme.nextFrame=" +  nextFrame.dumpAllPositions());
        } else {
        echo("viseme.nextFrame is null.  Are \"new\" visemes are properly configed on the server?");
        }
         */
        //  MPL TODO:  Send a servo command based on nextFrame positions, which are in absolute range-of-motion.
    }

    private void handleAnimationExecJob(AnimationExecJob aej) throws Throwable {

        if (true) {
            //TODO not this
            echo("IGNORING AnimationExecJob: ", aej);
            return;
        }
        Animation a = aej.getAnimation();
        String name = a.getName();
        int frameCount = a.getFrameCount();
        double framePeriod = a.getFramePeriodSeconds();
        echo("Starting playback for animation " + name + ", containing " + frameCount + " frames @ " + framePeriod
                + " seconds per frame");
        // Show all the joints used by this animation.
        echo("Used joints are: " + a.getUsedJointSet());
        // Dump the first few frames (safely, in case there's less than 3 frames).
        dumpLeadingFrames(a, 10);
        //  MPL TODO:  Start playing the frames of this animation on your timing loop.
        // Each frame is in relative (to previous-frame / current-position) range-of-motion coordinates.

        // Q:  Should this interrupt any animations already running?
        // In 2008 Hanson implementation, the answer was yes.
    }

    public void dumpLeadingFrames(Animation a, int numFrames) {
        int totalFrames = a.getFrameCount();
        if (numFrames > totalFrames) {
            numFrames = totalFrames;
        }
        for (int i = 0; i < numFrames; i++) {
            echo("Frame[" + i + "]=" + a.getFrameAt(i).dumpAllPositions());
        }
    }



    public static void main(String args[]) {
        try {
            CogbotJMXClient client = new CogbotJMXClient(serviceURL, new PrintWriter(System.out));
            client.echo("\nCreating a COGBOT-JMX-RMI connection to the Hanson RoboMonitorGUI application");
            client.connectProxies();
            // If we setup handlers before sending our messages, it slows down the
            // computer a lot, and the two text cues get spoken in reverse order.
            client.registerHandlers();
            if (args.length == 0) {
                client.testIntegroidProxyMethods();
                // Sleep for one year, allowing any notifications to be processed.
                Thread.sleep(365 * 24 * 60 * 60 * 1000);
            } else {
                if (args[0].equals("SAY")) {
                    if (args.length != 2) {
                        System.err.println("Usage:  [prog] SAY 'phrase to say'");
                    } else {
                        String phraseToSay = args[1];
                        client.sayText(phraseToSay);
                    }
                } else if (args[0].equals("HEAR")) {
                    Map<String, Double> heardMeanings = new HashMap();
                    for (int i = 1; i < args.length; i++) {
                        heardMeanings.put(args[i], 1.0);
                    }
                    client.hearMeanings(heardMeanings);
                }
            }

        } catch (Throwable t) {
            t.printStackTrace();
        }
    }

    public boolean isNotificationEnabled(Notification notification) {
        return isEnabled;
    }
    private void echo(String msg) {
        // debugPw.println(msg);
		theLogger.finest(msg);
    }
    private void echo(String string, Throwable e) {
        e.printStackTrace(debugPw);
        echo(string + e);
    }

    private void echo(String string, Object object) {
        if (object instanceof Throwable) {
            Throwable e = (Throwable) object;
            e.printStackTrace();
            echo(string + e);
            return;
        }
        try {
            echo(string + object);
        } catch (Exception e) {
        }
    }
}
