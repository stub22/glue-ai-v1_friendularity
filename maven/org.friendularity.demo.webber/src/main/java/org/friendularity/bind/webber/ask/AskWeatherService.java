/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.friendularity.bind.webber.ask;

import org.friendularity.webber.services.GenRespWithConf;
import org.friendularity.webber.services.INexusService;
import org.friendularity.webber.utils.Utils;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 *
 * @author matt
 */
public class AskWeatherService implements INexusService {

    public GenRespWithConf getResponse(String input) {
        GenRespWithConf best = new GenRespWithConf();
		Pattern weatherPattern = Pattern.compile("PL[.]([a-zA-Z_]+)$");
		Matcher weatherMatcher = weatherPattern.matcher(input);
		if(weatherMatcher.find()){
            Utils.println("ASK WEATHER: " + input);
			String askInput = weatherMatcher.group(1).replaceAll("_", " ");
			try{
				List<GenRespWithConf> askResponses = AskAskDotCom.GetWeather(askInput);
                best = Utils.getBest(askResponses);
			}catch(Exception e){}
		}
        return best;
    }

    public String getServiceName(){
        return "ASK_WEATHER";
    }

    public List<INexusService> getChildServices() {
        return new ArrayList<INexusService>();
    }

    public boolean ignoreBatchRequest() {
        return true;
    }
}
