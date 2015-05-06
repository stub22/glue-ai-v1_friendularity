/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.friendularity.weber.comm;

import org.friendularity.weber.utils.Utils;
import org.friendularity.bind.weber.cogbot.CogbotCommunicator;
import org.friendularity.weber.services.GenRespWithConf;
import org.friendularity.weber.config.MeneConfig;
import org.friendularity.weber.services.BatchServiceRequest;
import org.friendularity.weber.services.INexusService;
import org.friendularity.weber.utils.MeneLogger;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 *
 * @author matt
 */
public class Communicator implements CommunicatorMBean{
	public static String thePropsPath = "C:\\_hanson\\_deploy\\distro_20a\\conf\\_mene\\config.properties";

	private String myPropsPath = thePropsPath;
    private static long theMaxWaitTime = 5500L;
    private MeneConfig myConfig;
    private String myLastBest = null;
    private long myLastStart = 1;
    private long myLastFinish = 0;

    private MeneLogger myMeneLogger;

    private List<INexusService> myServices;
    private Map<String, INexusService> myServiceMap;

	public Communicator(){
        Utils.println("Initiating Messaging Nexus");
        myConfig = new MeneConfig();
        myConfig.load_configuration(myPropsPath);
        long batch_timeout;
        try{
            batch_timeout = Long.parseLong(myConfig.getBatchTimeout());
        }catch(Throwable t){
            t.printStackTrace();
            batch_timeout = 5000L;
        }
        BatchServiceRequest.setMaxWaitTime(batch_timeout);
        try {
            myMeneLogger=new MeneLogger(myConfig.getLogDirectory());
        }
        catch(java.io.IOException ex) {
            Utils.println("Could not create Mene log file.");
            ex.printStackTrace();
            myMeneLogger=null;
        }

        myServices = new ArrayList<INexusService>();
        //myServices.add(new ElbotCommunicator(myConfig));
		myServices.add(new CogbotCommunicator(myConfig));
        populateServiceMap(myServices);
	}

    private void populateServiceMap(List<INexusService> services){
        if(services == null)
            return;
        
        if(myServiceMap == null){
            myServiceMap = new HashMap<String, INexusService>();
        }
        for(INexusService s : services){
            myServiceMap.put("ANS-" + s.getServiceName(), s);
            populateServiceMap(s.getChildServices());
        }
    }

    public void startListening(){
        try {
            Thread.sleep(Long.MAX_VALUE);
        }
        catch (Exception e){
			e.printStackTrace();
		}
        Utils.println("Waiting for input");
    }

    private boolean resetConfig(String input){
        if(!input.equals(myConfig.getResetPhrase()))
            return false;

        Utils.println("Reloading Config");
        myConfig.loadConfigPropsAndVars(myPropsPath, true);
        return true;
    }

	public void communicate(String input, boolean toConvoid){
		myLastStart = System.currentTimeMillis();
        Utils.println("User Input: " + input);
        if(resetConfig(input))
            return;

        INexusService service = getServiceForInput(input);

        BatchServiceRequest bsr=null;
        if(service != null){
            bsr = handleSingleServiceInput(service, input, toConvoid);
        }

        if(bsr==null)  {
            bsr = new BatchServiceRequest(myServices, input);
        }
        bsr.requestResponses();
        GenRespWithConf resp = bsr.getBestResponse();

        setLastResponse(resp);
        Utils.println("Best Response[service=" + resp.getServiceName() +"]: " + resp.getResponse());
		Utils.println("-------------------------------------------------------------");
        //log all info encapsulated in bsr, as well as toConvoid
        if(myMeneLogger!=null){
            myMeneLogger.addBatchServiceRequest(bsr, toConvoid);
            myMeneLogger.appendToFile();
        }
	}

    private INexusService getServiceForInput(String input){
        if(input == null || !input.startsWith("ANS-"))
            return null;

        String service = input.split(" ")[0];
        if(myServiceMap.containsKey(service))
            return myServiceMap.get(service);

        return null;
    }

    private BatchServiceRequest handleSingleServiceInput(INexusService service, String input, boolean toConvoid){
        input = input.replace("ANS-" + service.getServiceName(), "").trim();

        BatchServiceRequest bsr = new BatchServiceRequest(service, input);
        bsr.requestResponses();
        GenRespWithConf resp= bsr.getBestResponse();

        if(resp == null)
            return null;

        return bsr;
    }

    private void setLastResponse(GenRespWithConf resp){
        myLastBest = resp.getResponse();
        myLastFinish = System.currentTimeMillis();
    }

    public String getLastBest(){
        Utils.println("Call to get last best");
        long start = System.currentTimeMillis();
        while(myLastStart > myLastFinish && (System.currentTimeMillis() - start) < theMaxWaitTime){
            try{
                Thread.sleep(100);
            }catch(Throwable t){}
        }
        Utils.println(myLastBest);
        String last = myLastBest;
        myLastBest = "";
		return last;
    }
}
