package org.friendularity.bind.weber.cogbot;

import org.friendularity.weber.services.INexusService;
import org.friendularity.weber.services.GenRespWithConf;
import org.friendularity.weber.comm.Communicator;

import java.net.*;
import java.io.*;

import org.friendularity.weber.config.MeneConfig;
import org.friendularity.weber.utils.Utils;
import java.util.ArrayList;
import java.util.List;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;
import org.friendularity.bind.cogbot.service.CogbotService;
import org.friendularity.bind.cogbot.simulator.CogbotAvatar;
import org.friendularity.bind.cogbot.slave.CogbotResponse;

public class CogbotCommunicator implements INexusService {

    // this is only called by get getID
    //private String lastKnownUserId = null;
    private String theBotId = "";
    private MeneConfig myConfig;
    private CogbotService cogbotService;
    static PrintWriter servicePw = new PrintWriter(new Writer() {

        public void write(char[] cbuf, int off, int len) throws IOException {
            Utils.println(new String(cbuf, off, len));
        }

        public void flush() throws IOException {
        }

        public void close() throws IOException {
        }
    });
    private final Properties myProperties;
    CogbotAvatar cogbotAvatar;

    public CogbotCommunicator(MeneConfig config) {
        myConfig = config;
        myProperties =  new Properties();
        try {
            myProperties.load(new FileReader("./resources/config.properties"));
        } catch (IOException ex) {
            Logger.getLogger(CogbotCommunicator.class.getName()).log(Level.SEVERE, null, ex);
        }
        theBotId = sanitizeId(myProperties.getProperty("robot_fullname","Bina 48"));
     //   lastKnownUserId = sanitizeId(myProperties.getProperty("default_username","UNKNOWN_PARTNER"));
        setBotProperty(CogbotService.cogbot_url_local, myConfig.getCogbotUrlLocal());
        HttpURLConnection.setFollowRedirects(true);
        cogbotService = CogbotService.getInstance(myProperties);
        cogbotService.setOutput(servicePw);
        cogbotAvatar = cogbotService.getDefaultAvatar(myProperties);
    }

    public GenRespWithConf getResponse(String input) {
        CogbotResponse cobotresp = getParsedResponse(input, false, 0);
        String resp = cobotresp == null ? null : cobotresp.getResponse();
        if (resp == null || resp.isEmpty()
                || resp.equals("No response to: " + input + ".")
                || resp.equals(CogbotResponse.NO_RESPONSE)) {
            return new GenRespWithConf("", -1);
        }
        int i9 =  cobotresp.getScore(1,12);
        return new GenRespWithConf(resp, i9);
    }

    private CogbotResponse getParsedResponse(String input, boolean clearingAssoc, int count) {
        CogbotResponse elRes = null;
        try {
            input = myConfig.getFormatter("input").format(input);
            elRes = cogbotService.getCogbotResponse(cogbotAvatar, servicePw, myProperties, input, theBotId);
        } catch (Exception e) {
            e.printStackTrace();
        }
        if (elRes != null) {
            theBotId = sanitizeId(elRes.getBotId());
            /*
            if (lastKnownUserId == null || lastKnownUserId.isEmpty()) {
                lastKnownUserId = sanitizeId(elRes.getUserId());
            }*/
            //Utils.println("Id = " + id + ", logId = " + logId);
            if (!clearingAssoc) {
            }
        }
        return elRes;
    }

    private String getResponseString(String input, boolean clearingAssoc, int count) {
        String resp = "";
        String animation = "";
        CogbotResponse elRes = getParsedResponse(input, clearingAssoc, count);
        if (elRes == null) {
            return "";
        }
        theBotId = sanitizeId(elRes.getBotId());
        resp = sanitizeResponse(elRes.getResponse());
        animation = sanitizeAnimation(elRes.getAnimation());
        /*
        if (lastKnownUserId == null || lastKnownUserId.isEmpty()) {
            lastKnownUserId = sanitizeId(elRes.getUserId());
        }*/
        //Utils.println("Id = " + id + ", logId = " + logId);
        if (!clearingAssoc) {
            resp = clearWordAssoc(input, resp, count);
        }
        resp = animation + " " + resp;
        return resp;
    }

    private String sanitizeResponse(String response) {
        response = myConfig.getFormatter("str").format(response);
        response = myConfig.getFormatter("partner").format(response);
        String temp = "";
        boolean modified = false;
        int start = 0, end = 0;
        for (int i = 0; i < response.length(); i++) {
            if (response.charAt(i) == '<') {
                start = i;
                modified = true;
            }
            if (response.charAt(i) == '>') {
                end = i;
                temp = response.substring(start, end + 1);
            }
        }


        if (modified) {
            response = response.replaceAll(temp, "");
        }
        return response;
    }

    private String sanitizeId(String tempId) {
        if (tempId==null) return "Bina Daxeline";
        tempId = tempId.trim();
        if (tempId.length()==0) tempId = "Bina Daxeline";
        tempId = myConfig.getFormatter("id").format(tempId);
        for (int i = 0; i < tempId.length(); i++) {
            if (tempId.charAt(i) == (char) '\"') {
                tempId = tempId.substring(i);
                break;
            }
        }
        return tempId;
    }

    private String sanitizeLogId(String tempId) {
        tempId = myConfig.getFormatter("logId").format(tempId);
        for (int i = 0; i < tempId.length(); i++) {
            if (tempId.charAt(i) == (char) '<') {
                tempId = tempId.substring(i);
                break;
            }
        }
        return tempId.replace("\"", "");
    }

    private String sanitizeAnimation(String animation) {
        animation = myConfig.getFormatter("animation").format(animation);
        for (int k = 0; k < animation.length(); k++) {
            if (animation.charAt(k) == (char) '\"') {
                animation = animation.substring(0, k);
                break;
            }
        }
        animation = myConfig.getFormatter("animation_list").format(animation);
        animation = animation.trim();
        if (animation.length() > 0) {
            animation = "<bookmark mark=\"anim:" + animation + "\" />";
        }

        return animation;
    }

    private String clearWordAssoc(String input, String resp, int count) {
        if (true) {
            return resp;
        }
        if (resp.matches(".*[Ww]ord associat.*")
                || resp.matches("^'[A-Za-z.\\- ]*'$")) {
            Utils.println("Stopping Word Association");
            if (count >= 3) {
                resp = "I could not understand what you said.";
            }
            getResponseString("stop playing word association", true, 0);
            getResponseString("change the subject", true, 0);
            resp = getResponseString(input.replaceAll("[Ww]ord%20associat(e|ion)", ""), false, count + 1);
        }
        return resp;
    }

    public String getServiceName() {
        return "COGBOT";
    }

    public List<INexusService> getChildServices() {
        return new ArrayList<INexusService>();
    }

    public boolean ignoreBatchRequest() {
        return false;
    }

    /*
    public String getId() {
        return lastKnownUserId;
    }*/

    public void setBotProperty(String name, String value) {
        myProperties.setProperty(name, value);
    }

    public void log(Throwable e) {
        e.printStackTrace(servicePw);
    }
}
