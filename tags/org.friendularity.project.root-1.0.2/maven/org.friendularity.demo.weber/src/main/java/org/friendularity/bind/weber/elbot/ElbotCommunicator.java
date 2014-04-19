package org.friendularity.bind.weber.elbot;

import org.friendularity.weber.services.INexusService;
import org.friendularity.weber.services.GenRespWithConf;

import java.net.*;
import java.io.*;
import org.friendularity.weber.config.MeneConfig;
import org.friendularity.weber.utils.Utils;
import java.util.ArrayList;
import java.util.List;

public class ElbotCommunicator implements INexusService{
	private String id = "";
	private String logId = "";
    private MeneConfig myConfig;
	private boolean myFirstRun;

	public ElbotCommunicator(MeneConfig config){
        id = "";
        logId = "";
        myFirstRun = true;
        myConfig = config;
        HttpURLConnection.setFollowRedirects(true);
	}

    public GenRespWithConf getResponse(String input){
        String resp = getResponseString(input, false, 0);
        GenRespWithConf best = new GenRespWithConf(resp, 7);
        //Utils.println("Elbot Response(" + best.confidence + "): " + best.response);
        return best;
    }


    private String getResponseString(String input, boolean clearingAssoc, int count) {
        String resp = "";
        String animation = "";
        ElbotResponse elRes = null;
        try {
            input = myConfig.getFormatter("input").format(input);
            elRes = getParsedResponse(input);
        } catch (Exception e) {
            e.printStackTrace();
        }
        if (elRes == null) {
            return "";
        }
        myFirstRun = false;
        logId = elRes.getLogId();
        resp = elRes.getResponse();
        animation = elRes.getAnimation();
        if (id.isEmpty()) {
            id = elRes.getId();
        }
        //Utils.println("Id = " + id + ", logId = " + logId);
        if (!clearingAssoc) {
            resp = clearWordAssoc(input, resp, count);
        }
        resp = animation + resp;
        return resp;
    }

    private ElbotResponse getParsedResponse(String input){
        boolean web = false;
        BufferedReader reader = getReaderForUrl(input, myConfig.getElbotUrlLocal());
        if(reader == null){
            reader = getReaderForUrl(input, myConfig.getElbotUrlRemote());
            web = true;
        }
        return new ElbotResponse(reader, myConfig, web);
    }

    private BufferedReader getReaderForUrl(String input, String strUrl){
        URL url = getElbotUrl(input, strUrl, id, logId, myFirstRun);
        return getElbotReader(url);
    }

    private URL getElbotUrl(String input, String baseUrl, String id, String logId, boolean firstRun){
        URL url = null;
        try{
            url = new URL(baseUrl + "entry=" + input);
            if(firstRun){
                url = new URL(baseUrl + "START=normal");
            }
            else if(!id.equals("")){
                url = new URL(baseUrl + "ident=" + id + "&userlogid=" + logId + "&entry=" + input);
            }
            else if(id.equals("")){
                url = new URL(baseUrl + "entry=" + input);
            }
            else{
                id = "";
                url = new URL(baseUrl + "entry=" + input);
            }
			Utils.println("Sending to: " + url.toString());
        }
        catch(Exception e){
            e.printStackTrace();
        }
        return url;
    }

    private BufferedReader getElbotReader(URL url){
        try{
            return new BufferedReader(new InputStreamReader(url.openStream()));
        }
        catch(Exception ex){
            return null;
        }
    }

    private String clearWordAssoc(String input, String resp, int count){
		if(resp.matches(".*[Ww]ord associat.*") || resp.matches("^'[A-Za-z.\\- ]*'$")){
			Utils.println("Stopping Word Association");
			if(count >= 3)
			{
				resp = "I could not understand what you said.";
			}
			getResponseString("stop playing word association", true, 0);
			getResponseString("change the subject", true, 0);
			resp = getResponseString(input.replaceAll("[Ww]ord%20associat(e|ion)", ""), false, count + 1);
		}
        return resp;
    }

    public String getServiceName(){
        return "ELBOT";
    }

    public List<INexusService> getChildServices() {
        return new ArrayList<INexusService>();
    }

    public boolean ignoreBatchRequest() {
        return false;
    }
}
