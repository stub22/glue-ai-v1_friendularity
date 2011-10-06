/*
 * A function for asking question from ask.com
 *
 * Author: Ali Rafiee
 */

package org.friendularity.bind.webber.ask;
import org.friendularity.webber.services.INexusService;
import org.friendularity.webber.services.GenRespWithConf;
import org.friendularity.webber.services.BatchServiceRequest;

import org.friendularity.webber.config.MeneConfig;
import org.friendularity.gui.webber.MenePanel;
import org.friendularity.webber.utils.Utils;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.*;
import java.util.*;
import java.util.ArrayList;


/**
 *
 * @author alir
 */
public class AskAskDotCom implements INexusService {
    public static MeneConfig myConfig = null;
    public static MenePanel myPanel = null;
    private static List<INexusService> theAskServices;

    public static void ensureInitialized(){
        if(theAskServices == null){
            theAskServices = new ArrayList<INexusService>();
            theAskServices.add(new AskDotComService());
            theAskServices.add(new AskWeatherService());
            //theAskServices.add(new AskDefinitionService());
        }
    }

    public static LinkedList<GenRespWithConf> Ask(String question) throws Exception{
        LinkedList<GenRespWithConf> resultList = new LinkedList<GenRespWithConf>();

        //first hit the server for the search results
        StringBuilder htmlResult = new StringBuilder();
        question = URLEncoder.encode(question,"UTF-8");
        String requestUrl = "http://www.ask.com/web?q="+question;
        try
        {
            URL url = new URL(requestUrl.toString());
            BufferedReader in = new BufferedReader(new InputStreamReader(url.openStream()));
            String inputLine;
            while ((inputLine = in.readLine()) != null)
            {
                htmlResult.append(inputLine);
            }
            in.close();

        }
        catch (IOException e)
        {
            e.printStackTrace();
        }
        GenRespWithConf response = getAskResponse(htmlResult);
        if(response != null){
            resultList.add(response);
            return resultList;
        }
        response = getSecondaryResponse(htmlResult);
        if (response != null){
            resultList.add(response);
            return resultList;
        }
        response = parseBusinessListingResposne(htmlResult);
        if (response != null){
            resultList.add(response);
            return resultList;
        }
        response = parseDefinitionResposne(htmlResult);
        if (response != null){
            resultList.add(response);
            return resultList;
        }
        response = parseWeatherResposne(htmlResult);
        if (response != null){
            resultList.add(response);
            return resultList;
        }
        response = GetTime(htmlResult);
        if (response != null){
            resultList.add(response);
            return resultList;
        }
        response = getMiscAnswers(htmlResult);
        if (response != null){
            resultList.add(response);
            return resultList;
        }
        return resultList;
    }

    private static GenRespWithConf getAskResponse(StringBuilder htmlResult){
        // I have seen multiple answer classes
        // answers_ui_t16, answers_ui_t20, and I imagine there's some fill in
        // So just grab the start of the span and fidn the first '>' after that
        int startd = htmlResult.indexOf("<span class=\"answers_ui");
        int starta = htmlResult.indexOf("<a class=\"answers_ui");
        if(startd == -1 && starta == -1){
            return null;
        }
        int start = -1;
        if(starta == -1 || startd ==-1){
            start = Math.max(starta, startd);
        }else{
            start = Math.min(starta,startd);
        }
        if(start == -1){
            return null;
        }
        start = htmlResult.indexOf(">",start) + 1;
        int end = htmlResult.indexOf("<div class=\"answers_ui_line\" />", start);
        if(end == -1){
            end = htmlResult.indexOf("<span class=\"T11\">Source: </span>", start);
            if(end == -1){
                return null;
            }
        }
        String s = htmlResult.substring(start, end);
        if(s.isEmpty()){
            return null;
        }
        s = sanitizeAskAnswer(s);
        return new GenRespWithConf(s, 10);
    }

    private static GenRespWithConf getSecondaryResponse(StringBuilder htmlResult){
        //This wierd response came up when I asked "Define weather"
        int start = htmlResult.indexOf("<span class=\"T29 dafs2\">");
        if(start == -1){
            return null;
        }
        start = htmlResult.indexOf(">",start) + 1;
        int end = htmlResult.indexOf("<span class=\"T11\">Source: </span>", start);
        if(end == -1){
            return null;
        }
        String s = htmlResult.substring(start, end);
        if(s.isEmpty()){
            return null;
        }
        s = sanitizeAskAnswer(s);
        return new GenRespWithConf(s, 10);
    }

    private static GenRespWithConf GetTime(StringBuilder htmlResult) throws Exception{
        //look for time tags
        int index = htmlResult.indexOf("<span class=\"T6 b\">");
        if (index != -1)
        {
            int endIndex = htmlResult.indexOf("</table></div>",index);
            if (endIndex != -1)
            {
                String result = htmlResult.substring(index,endIndex).toString().replaceAll("\\<.*?\\>", " ");
				if(myConfig != null){
					result = myConfig.getFormatter("states").format(result);
				}
				result = result.replaceAll("\\d{5}", ""); // remove zipcodes from the response
				result = result.replaceAll("\\s+[:]\\d+", ""); // remove seconds
				result = result.replaceAll("\\s[A-Z]{3}\\s", ""); // remove timezone
                GenRespWithConf response = new GenRespWithConf(sanitizeAskAnswer(result),10);
                return response;
            }
        }
        return null;
    }

    private static GenRespWithConf getMiscAnswers(StringBuilder htmlResult) throws Exception{
        int index = htmlResult.indexOf("<div class=\"KonaBody\">");
        if (index == -1){
            return null;
        }
        index = htmlResult.indexOf("answers_ui",index);
        if (index == -1){
            return null;
        }
        index = htmlResult.indexOf("<div id=\"r0_a",index);
        if (index == -1){
            index = htmlResult.indexOf("<div id=\"r1_a",index);
            if (index == -1){
                index = htmlResult.indexOf("id=\"r1_a",index);
                if (index == -1){
                    index = htmlResult.indexOf("id=\"r1_a",index);
                    if (index == -1){
                        return null;
                    }
                }
            }
        }
        index = htmlResult.indexOf(">",index) + 1;
        if (index == -1){
            return null;
        }
        int endIndex = htmlResult.indexOf("</div>",index);
        if (endIndex == -1){
            return null;
        }
        String result = sanitizeAskAnswer(htmlResult.substring(index,endIndex));
        GenRespWithConf response = new GenRespWithConf(result,6);
        return response;
    }

    private static GenRespWithConf parseDefinitionResposne(StringBuilder htmlResult){
        int index = htmlResult.indexOf("- Definition");
        if (index == -1){
            return null;
        }
        index = htmlResult.indexOf("<table>",index);
        if (index == -1){
            return null;
        }
        index = htmlResult.indexOf("<td>",index);
        if (index == -1){
            return null;
        }
        int endIndex = htmlResult.indexOf("</td>",index);
        if (endIndex == -1){
            return null;
        }
        String result = sanitizeAskAnswer(htmlResult.substring(index,endIndex));
        GenRespWithConf response = new GenRespWithConf(result,9);
        return response;
    }

    private static LinkedList<GenRespWithConf> GetWikiAnswer(String link) throws Exception{
        LinkedList<GenRespWithConf> resultList = new LinkedList<GenRespWithConf>();

        //first hit the server for the search results
        StringBuilder htmlResult = new StringBuilder();
        try
        {
            URL url = new URL(link);
            BufferedReader in = new BufferedReader(new InputStreamReader(url.openStream()));
            String inputLine;
            while ((inputLine = in.readLine()) != null)
            {
                htmlResult.append(inputLine);
            }
            in.close();

        }
        catch (IOException e)
        {
            e.printStackTrace();
        }

        int index = htmlResult.indexOf("q_answer");
        if (index != -1)
        {
            index = htmlResult.indexOf(">",index);
            if (index != -1)
            {
                index++;
                int endIndex = htmlResult.indexOf("</div>", index);
                if (endIndex != -1)
                {
                    String results = htmlResult.substring(index,endIndex).toString().replaceAll("\\<.*?\\>", " ");
                    GenRespWithConf response = new GenRespWithConf(sanitizeAskAnswer(results),9);
                    resultList.add(response);
                }
            }
        }
        return resultList;
    }

    public static LinkedList<GenRespWithConf> GetWeather(String location) throws Exception{
        LinkedList<GenRespWithConf> resultList = new LinkedList<GenRespWithConf>();

        //first hit the server for the search results
        StringBuilder htmlResult = new StringBuilder();
        location = URLEncoder.encode("weather " + location,"UTF-8");
        String requestUrl = "http://www.ask.com/web?q="+location;
        try
        {
            URL url = new URL(requestUrl.toString());
            BufferedReader in = new BufferedReader(new InputStreamReader(url.openStream()));
            String inputLine;
            while ((inputLine = in.readLine()) != null)
            {
                htmlResult.append(inputLine);
            }
            in.close();
        }
        catch (IOException e)
        {
            e.printStackTrace();
        }
        GenRespWithConf resp = parseWeatherResposne(htmlResult);
        if(resp != null){
            resultList.add(resp);
        }
        return resultList;
    }

    private static GenRespWithConf parseWeatherResposne(StringBuilder htmlResult){
        int index = htmlResult.indexOf("Current Weather in");
        if (index == -1){
            return null;
        }
        int endIndex = htmlResult.indexOf("|</span>",index);
        if (endIndex == -1){
            return null;
        }
        String result = sanitizeAskAnswer(htmlResult.substring(index,endIndex));
        if(myConfig != null){
            result = myConfig.getFormatter("states").format(result);
        }
        result = result.replaceAll("\\d{5}", ""); // remove zipcodes from the response
        GenRespWithConf response = new GenRespWithConf(result,10);
        return response;
    }

    private static GenRespWithConf parseBusinessListingResposne(StringBuilder htmlResult){
        int index = htmlResult.indexOf("Business Listings for");
        if (index == -1){
            return null;
        }
        index = htmlResult.indexOf("1.</td>",index);
        if (index == -1){
            return null;
        }
        index = htmlResult.indexOf("<td",index);
        if (index == -1){
            return null;
        }
        int endIndex = htmlResult.indexOf("</td>",index);
        if (endIndex == -1){
            return null;
        }
        String result = sanitizeAskAnswer(htmlResult.substring(index,endIndex));
        if(myConfig != null){
            result = myConfig.getFormatter("states").format(result);
        }
        GenRespWithConf response = new GenRespWithConf(result,10);
        return response;
    }

    public GenRespWithConf getResponse(String input){
        ensureInitialized();
        BatchServiceRequest bsr = new BatchServiceRequest(theAskServices, input);
        bsr.requestResponses();
        return bsr.getBestResponse();
    }

    protected static GenRespWithConf getBestStandard(String input){
        Utils.println("ASK: " + input);
        GenRespWithConf best = new GenRespWithConf();
		try{
			String askInput = input.replaceAll("_", " ");
			List<GenRespWithConf> askResponses = AskAskDotCom.Ask(askInput);
			best = Utils.getBest(askResponses);
		}catch(Exception e){}
        return best;
    }

    public String getServiceName(){
        return "ASK_DOT_COM";
    }

    public List<INexusService> getChildServices(){
        ensureInitialized();
        return theAskServices;
    }

    public boolean ignoreBatchRequest() {
        return false;
    }

    private static String sanitizeAskAnswer(String answer){
        answer = answer.replaceAll("<[^>]+>", " ");//Strip html tags
        answer = answer.replaceAll("\\s\\s+", " ");//remove duplicate spaces
        answer = myConfig.getFormatter("ask_replace").format(answer);
        return answer;
    }

}
