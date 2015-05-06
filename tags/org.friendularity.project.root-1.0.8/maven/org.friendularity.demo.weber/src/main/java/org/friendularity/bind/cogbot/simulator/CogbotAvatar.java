/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.friendularity.bind.cogbot.simulator;


import org.cogchar.integroid.jmxwrap.IntegroidWrapperMXBean;

import java.io.PrintWriter;
import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.management.AttributeChangeNotification;
import javax.management.Notification;
import javax.management.NotificationListener;
import org.apache.http.client.HttpClient;
import org.friendularity.bind.cogbot.service.CogbotService;
import org.friendularity.bind.cogbot.cogsim.CogSimConf;
import org.friendularity.bind.cogbot.cogsim.CogSimOp;
import org.friendularity.bind.cogbot.cogsim.CogSimBridge;
import org.friendularity.bind.cogbot.cogsim.DictationReciever;
import org.friendularity.bind.cogbot.jmx.CogbotJMXClient;
import org.friendularity.bind.cogbot.scripting.CogbotPrimitive;
import org.friendularity.bind.cogbot.scripting.ObjectLispWriter;
import org.friendularity.bind.cogbot.scripting.SerialEventQueue;
import org.cogchar.integroid.awareness.AwarenessConstants;
import org.cogchar.integroid.awareness.AwarenessHelpFuncs;
import org.cogchar.zzz.api.platform.cues.VariableCue;
import org.cogchar.zzz.platform.stub.CueStub;
import org.cogchar.zzz.platform.stub.JobStub;

/**
 *
 * @author Administrator
 */
public class CogbotAvatar implements NotificationListener, Serializable {

    public transient boolean isCogSimEnabled = false;
    public boolean isPolling = false;
    
    private static Logger theLogger = Logger.getLogger(CogbotAvatar.class.getName());
    transient CogbotService service;
    CogSimConf myCogSimConf;
    transient private CogSimBridge myCSB;
    final Properties config;
    transient final PrintWriter debugPw;
    transient Map<String, CogbotPrimitive> primitives = new HashMap<String, CogbotPrimitive>();
    transient CogbotJMXClient cogbotJMXClient;
    public boolean debugPrintJMX = false;
    final SerialEventQueue TODO_QUEUE;

    //public String USER_PARTNER = null;
    //public String UNKNOWN_PARTNER = "UNKNOWN_PARTNER";


    public String getBotId() {
        return config.getProperty("cogbot_name","");
    }

    public CogSimConf getConfig() {
        return myCogSimConf;
    }

    public CogbotAvatar(CogbotService service0) {
        service = service0;
        debugPw = service0.getLogPrintWriter();
        config = service0.getProperties();
        TODO_QUEUE = new SerialEventQueue(getBotId());
        myCogSimConf = service0.getConf();
        readProperties(config);
    }

    public synchronized void registerListener(DictationReciever dictationReciever) {
        myCSB = new CogSimBridge(this, dictationReciever, myCogSimConf);        
        if (myCSB.isConfigured()) {
            Thread t = new Thread(myCSB);
            t.start();
        } else {
            theLogger.warning("CogSim connection is not configured, so no connection will be made to CogSim");
        }

        warnSettings();
    }

    public synchronized void postActionReqToCogbot(String verb, String details, boolean debugFlag) {
        if (!isCogSimEnabled) return;
        CogSimOp cso = makeCogSimOp(myCogSimConf, null);
        try {
            cso.postActionReqToCogbot(verb, details, debugFlag);
        } catch (Throwable t) {
            theLogger.log(Level.WARNING, "Cannot send cogbot-doit command[" + verb + ", " + details + "]", t);
        }
    }

    public synchronized void ensureJMX() {
        if (CogbotService.disableJMX) {
            return;
        }
        try {
            if (cogbotJMXClient == null) {
                cogbotJMXClient = new CogbotJMXClient(config.getProperty("character_engine_jmx_url", CogbotJMXClient.serviceURL), debugPw);
                cogbotJMXClient.registerListener((NotificationListener) this);
            }
        } catch (Throwable ex) {
            theLogger.log(Level.SEVERE, null, ex);
        }
    }

    public synchronized boolean isOnline() {
        ensureJMX();
        return true;
    }

    public synchronized void readProperties(Properties meneProps) {
        CogbotService.disableJMX = Boolean.valueOf(meneProps.getProperty("cogbot_jmx_disable", "" + CogbotService.disableJMX));

        // todo rename in config?
        if (Boolean.valueOf(config.getProperty("cogsim_enable", "" + isCogSimEnabled))) {
            isCogSimEnabled = true;
        }
        if (Boolean.valueOf(config.getProperty("cogsim_poll_enabled", "" + isPolling))) {

            isPolling = true;
        }

        theLogger.info("$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$");
        if (meneProps != null) {
            myCogSimConf.readProperties(meneProps);
            if (myCogSimConf.isConfigured()) {
            }
        } else {
            throw new RuntimeException("Can't load cogsim properties!");
        }
        theLogger.info("$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$");
        ensureJMX();
    }

    public synchronized String fetchLastThingWeSaid(boolean debugFlag) {
        if (!isPolling) return "";
        CogSimOp cso = makeCogSimOp(myCogSimConf, null);
        try {
            return cso.fetchLastThingWeSaid(debugFlag);
        } catch (Throwable t) {
            service.log(Level.WARNING, "Cannot fetchLastThingWeSaid []", t);
            return "";
        }
    }

    public synchronized String fetchLastThingWeHeard(boolean debugFlag) {
        if (!isPolling) return "";
        CogSimOp cso = makeCogSimOp(myCogSimConf, null);
        try {
            return cso.fetchLastThingWeHeard(debugFlag);
        } catch (Throwable t) {
            service.log(Level.WARNING, "Cannot fetchLastThingWeHeard []", t);
            return "";
        }
    }

    public void registerAction(CogbotPrimitive cogbotPrimitive) {
        synchronized (primitives) {
            primitives.put(cogbotPrimitive.getName(), cogbotPrimitive);
        }
    }

    public void handleNotification(Notification notification, Object handback) {
        //throw new UnsupportedOperationException("Not supported yet.");
        debugJMX("----->" + notification);
        try {
            if (notification instanceof AttributeChangeNotification) {
                AttributeChangeNotification acn = (AttributeChangeNotification) notification;
                String attribName = acn.getAttributeName();
                String attribTypeName = acn.getAttributeType();
                Object newValue = acn.getNewValue();
                Object oldValue = acn.getOldValue();

                // Dispatch based on "attribute name"
                if (attribName.equals(IntegroidWrapperMXBean.ATTRIB_CUE_POSTED)
						|| attribName.equals(IntegroidWrapperMXBean.ATTRIB_CUE_UPDATED)){
                    if (newValue instanceof CueStub) {
                        CueStub cue = (CueStub) newValue;
                        handlePostedOrUpdatedCue(cue);
                    }
                } else if (attribName.equals(IntegroidWrapperMXBean.ATTRIB_CUE_CLEARED)) {
                    if (newValue instanceof CueStub) {
                        CueStub cue = (CueStub) newValue;
                        debugJMX("Ignoring cueCleared notification");
                    }
                } else if (attribName.equals(IntegroidWrapperMXBean.ATTRIB_JOB_POSTED)) {
                    if (newValue instanceof JobStub) {
                        JobStub job = (JobStub) newValue;
                        //handlePostedJob(job);
                    } else {
                        // handlePostedJobObject(newValue);
                    }
                } else if (attribName.equals(IntegroidWrapperMXBean.ATTRIB_JOB_CLEARED)) {
                    //  Job job = (Job) newValue;
                    debugJMX("Ignoring jobCleared notification");
                } else {
                    debugJMX("####################################################################");
                    debugJMX("Unhandled attribute change notification.  Details are:");
                    debugJMX("\tAttributeName: " + attribName);
                    debugJMX("\tAttributeType: " + attribTypeName);
                    debugJMX("\tNewValue: " + newValue);
                    debugJMX("\tOldValue: " + oldValue);
                    debugJMX("####################################################################");
                }
            }
            sendNotification(notification);
            debugJMX("\nFinished processing notification at:" + System.currentTimeMillis());
            debugJMX("******************");
        } catch (Throwable t) {
            t.printStackTrace();
        }


        // new Throwable().printStackTrace();
        // System.err.println("----->"+notification);

    }

    void echo(String msg) {
        if (debugPw==null) return;
        debugPw.println(msg);
    }

    synchronized void handlePostedOrUpdatedCue(CueStub cue) {
        if (cue instanceof VariableCue) {
            VariableCue vc = (VariableCue)cue;
            String varName = vc.getName();
/*
            if (name!=null && name.equalsIgnoreCase("PARTNER")) {
                setLookingAt(vc.getValue());
            }
 */
			if (varName!=null && varName.equalsIgnoreCase(AwarenessConstants.VAR_GAZE_PERSON_DESC)) {
				String gazePersonDesc = vc.getValue();
				String cogbotUsersName = gazePersonDesc;
				if (gazePersonDesc.equals(AwarenessConstants.DESC_BOGEY)) {
					cogbotUsersName = "UNRECOGNIZED";
				} else if (gazePersonDesc.equals(AwarenessConstants.DESC_NOBODY)) {
					cogbotUsersName = "UNSEEN";
				}

				setLookingAt(cogbotUsersName);
			}
        }
    }

    public String getResponse(String input, String from) {
        String id = getBotId();
        return service.getCogbotResponse(this,debugPw, config, input, from, id).getResponse();
    }

    synchronized  void setLookingAt(String value) {
		AwarenessHelpFuncs.logAware("CogbotAvatar.setLookingAt(" + value + ")");
        // we lloked away maybe
/*        if (value == null) {
            return;
        }
        // not "me"
        value = value.trim();
        if (value.length() < 3) {
            return;
        }
        String old = USER_PARTNER;        
        USER_PARTNER = value;/*
 */
        postActionReqToCogbot("aiml","@chuser " + value, true);// + " - "+ old, true);
    }

    private void debugJMX(String string) {
        if (!debugPrintJMX) return;
        echo("CogbotJMX: " + string);
    }

/*
    public String coerceToUser(String userName) {
        if (!isEmpty(userName)) {
            if (isEmpty(USER_PARTNER)) {
                USER_PARTNER = userName;
            }
        }
        return USER_PARTNER;
    }
*/
    static boolean isEmpty(String user) {
        return user == null || user.trim().isEmpty();
    }

    private CogSimOp makeCogSimOp(CogSimConf myCogSimConf, HttpClient object) {
        return new CogSimOp(this, myCogSimConf, object);
    }

    public void InvokeSerialAction(Runnable runnable) {
        TODO_QUEUE.invokeLater(runnable);
    }

    public void warnSettings() {
        if (!isCogSimEnabled) {
            warning("isCogSimEnabled = " + isCogSimEnabled + " so Cogbot may not know what the user is responding to");
        }
        if (!isPolling) {
            warning("isPolling = " + isPolling + " so we will not recivie Sim conversation ");
        }
    }

    private void warning(String string) {
        theLogger.warning(string);
        string = "WARNING: " + string;
        System.err.println(string);
        echo(string);
    }
    synchronized void sendNotification(Notification acn) {
        if (true) return;
        String evnt = ObjectLispWriter.makeLispObject(acn);
        postActionReqToCogbot("aiml","@fromjmx " + evnt, false);
    }
}
