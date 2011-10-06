/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.friendularity.bind.webber.elbot;

import org.friendularity.webber.config.MeneConfig;
import org.friendularity.webber.utils.FileHelper;
import java.io.BufferedReader;
import java.io.File;
import java.util.concurrent.TimeUnit;

/**
 *
 * @author matt
 */
public class ElbotResponse {
    private String myId;
    private String myLogId;
    private String myResponse;
    private String myAnimation;
    private boolean isWebQuery;

    private MeneConfig myConfig;

    public ElbotResponse(BufferedReader reader, MeneConfig config, boolean webQuery){
        myConfig = config;
        isWebQuery = webQuery;
        parseHTML(reader);
    }

    private void parseHTML(BufferedReader in){
        String temp = "";
        String response = "";
        String tempId = "";
        String tempLogId = "";
        String animation = "";

        boolean accept = false;
        boolean anim = true;

        boolean idAccept = false;
        boolean animationAccept = false;
        //read through the HTML data until the end
        try{
            while ((temp = in.readLine()) != null){
                //get the id information
                if(temp.contains("<form action")){
                    idAccept = true;
                }
                if(temp.contains("EXTRAINPUT")){
                    idAccept = false;
                }
                if(idAccept == true){
                    tempId = in.readLine();
                    tempLogId = in.readLine();
                }
                if(temp.contains("<tr>") && anim){
                    animationAccept = true;
                }
                if(animationAccept){
                    String t = in.readLine();
                    animation += t;
                }
                if(temp.contains("img src")){
                    animationAccept = false;
                    anim = false;
                }
                if(temp.contains("<!-- Begin Response !-->"))
                    accept = true;
                if(accept){
                    String t = in.readLine();
                    response += t;
                }
                if(temp.contains("<!-- End Response !-->")){
                    accept = false;
                    break;
                }
            }
            setId(tempId);
            setLogId(tempLogId);
            setAnimation(animation);
            setResponse(response);
        }
        catch(Throwable x){
            x.printStackTrace();
        }
    }

    private String sanitizeId(String tempId){
        tempId = myConfig.getFormatter("id").format(tempId);
        for(int i=0; i < tempId.length(); i++){
            if(tempId.charAt(i) == (char)'\"'){
                tempId = tempId.substring(i);
                break;
            }
        }
        return tempId;
    }

    private String sanitizeLogId(String tempId){
        tempId = myConfig.getFormatter("logId").format(tempId);
        for(int i=0; i < tempId.length(); i++){
            if(tempId.charAt(i) == (char)'<'){
                tempId = tempId.substring(i);
                break;
            }
        }
        return tempId.replace("\"", "");
    }

    private String sanitizeAnimation(String animation){
        animation = myConfig.getFormatter("animation").format(animation);
        if(isWebQuery){
            animation = animation.substring(15);
        }
        else{
            animation = animation.substring(18);
        }
        for(int k=0; k < animation.length(); k++){
            if(animation.charAt(k) == (char)'\"'){
                animation = animation.substring(0, k);
                break;
            }
        }
        animation = myConfig.getFormatter("animation_list").format(animation);
        if(animation.length() > 0)
            animation = "<bookmark mark=\"anim:" + animation + "\" />";
        
        return animation;
    }

    private String sanitizeResponse(String response){
        response = myConfig.getFormatter("str").format(response);

        //this only grabs the last <*> and leaves anything else
        // do we want it to strip all tags? - matts
        String temp = "";
        boolean modified = false;
        int start = 0, end = 0;
        for (int i = 0; i < response.length(); i++){
            if (response.charAt(i) == '<'){
                start = i;
                modified = true;
            }
            if (response.charAt(i) == '>'){
                end = i;
                temp = response.substring(start, end+1);
            }
        }

        if(modified){
            response = response.replaceAll(temp, "");
        }
        return response;
    }

    public String getAnimation() {
        return myAnimation;
    }

    public void setAnimation(String animation) {
        myAnimation = sanitizeAnimation(animation);
    }

    public String getId() {
        return myId;
    }

    public void setId(String id) {
        myId = sanitizeId(id);
    }

    public String getLogId() {
        return myLogId;
    }

    public void setLogId(String logId) {
        myLogId = sanitizeLogId(logId);
    }

    public String getResponse() {
        return myResponse;
    }

    public void setResponse(String response) {
        myResponse = sanitizeResponse(response);
    }


}
