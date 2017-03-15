/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.friendularity.bind.cogbot.cogsim;

//import com.hansonrobotics.mene.Communicator;
//import com.hansonrobotics.mene.config.MeneConfig;
import java.io.Serializable;
import java.util.Properties;
import java.util.logging.Logger;
import org.friendularity.bind.cogbot.service.CogbotService;

/**
 *
 * @author Stu Baurmann
 */
public class CogSimConf implements Serializable {
    private final Properties myProps;
    private int cogsim_poll_ms = 1000;

    boolean isWaiting() {
        return waiting;
    }
    
    void setWaiting( boolean w) {
        waiting = w;
    }

    /**
     * @return the urlRoot
     */
    public String getUrlRoot() {
        return urlRoot;
    }

        /**
     * @return the urlRoot
     */
    public String getChatUrl() {
        return urlRoot + "?";
    }

    /**
     * @param urlRoot the urlRoot to set
     */
    public void setUrlRoot(String urlRoot) {
        this.urlRoot = urlRoot;
    }

    public void setIp(String string) {
        setUrlRoot("http://"+string+":5580/");
        waiting = false;
    }

    public long getPollSleepTime() {
        // cant be 0 = forever or too fast
        return (cogsim_poll_ms < 100) ? 100 : cogsim_poll_ms;
    }

    public enum Op {

        GET_SAID,
        GET_HEARD,
        DO_ACTION
    }
    private static Logger theLogger = Logger.getLogger(CogSimConf.class.getName());
    // package/protected scope
    //defualt
    String urlRoot = "http://127.0.0.1:5580/";
    String saidURL_tail = "posterboard/onchat-said";
    String heardURL_tail = "posterboard/onchat-heard";
    String actionURL_tail = "postaction";
    boolean waiting = true;

    public CogSimConf(Properties config) {
        myProps = config;
        readProperties(config);
    }

    public String valOrDefault(String val, String def) {
        return (val != null) ? val : def;
    }

    public void readProperties(Properties config) {
        theLogger.info("Props=" + config);
        if (myProps != null) {
            myProps.putAll(config);
            config = myProps;
        }
        setUrlRoot(config.getProperty("cogsim_url_root", getUrlRoot()));
        saidURL_tail = config.getProperty("cogsim_said_url_tail", saidURL_tail);
        heardURL_tail = config.getProperty("cogsim_heard_url_tail", heardURL_tail);
        actionURL_tail = config.getProperty("cogsim_act_url_tail", actionURL_tail);
        try {
            cogsim_poll_ms = Integer.valueOf(config.getProperty("cogsim_poll_ms", "" + cogsim_poll_ms));
        } catch (NumberFormatException nfe) {
        }
    }

    public String findOpURL(Op op) {
        if (getUrlRoot() == null) {
            // return null;
            throw new RuntimeException("Cannot find URL becuase urlRoot=" + getUrlRoot());
        }
        switch (op) {
            case GET_HEARD:
                return getUrlRoot() + heardURL_tail;
            case GET_SAID:
                return getUrlRoot() + saidURL_tail;
            case DO_ACTION:
                return getUrlRoot() + actionURL_tail;
            default:
                return "NO_OP_FOR_" + op;
        }
    }

    public boolean isSet(String val) {
        return ((val != null) && (val.length() > 0));
    }

    public boolean isConfigured() {
        return isSet(getUrlRoot()) && isSet(heardURL_tail) && isSet(saidURL_tail);
    }
}
