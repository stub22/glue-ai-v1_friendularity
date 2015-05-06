/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.friendularity.weber.services;

import java.util.ArrayList;
import java.util.List;
import org.friendularity.weber.utils.Utils;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author matt
 */
public class BatchServiceRequest {
	private static Logger	theLogger = Logger.getLogger(BatchServiceRequest.class.getName());

    private static long theMaxWaitTime = 5000L;
    private List<INexusService> myServices = new ArrayList<INexusService>();
    private String myInput;

    public static void setMaxWaitTime(long wait){
        theMaxWaitTime = wait;
    }

    public String getInputString()  {
        return myInput;
    }


    private GenRespWithConf       myBestResponse;
    public GenRespWithConf getBestResponse()  {
        requestResponses();        
        return myBestResponse;
    }
    
    private List<GenRespWithConf> myReceivedResponses;
    public List<GenRespWithConf> getReceivedResponses(){
        requestResponses();
        return new ArrayList<GenRespWithConf>(myReceivedResponses);
    }

    private List<INexusService> myIgnoredServices = new ArrayList<INexusService>();
    public List<INexusService> getIgnoredServices()  {
        return new ArrayList<INexusService>(myIgnoredServices);
    }

    private List<ServiceRequestThread> myRequests = new ArrayList<ServiceRequestThread>();
    public List<ServiceRequestThread> getAllRequests()  {
        return new ArrayList<ServiceRequestThread>(myRequests);
    }

    private List<ServiceRequestThread> myCompletedRequests;
    public List<ServiceRequestThread> getCompletedRequests()  {
        requestResponses();
        return new ArrayList<ServiceRequestThread>(myCompletedRequests);
    }

    private List<ServiceRequestThread> myIncompleteRequests;
    public List<ServiceRequestThread> getIncompleteRequests()  {
        requestResponses();
        return new ArrayList<ServiceRequestThread>(myIncompleteRequests);
    }

    private Long myStartTimeMillis;
    public synchronized Long getStartTimeMillis() {
        return myStartTimeMillis;
    }
    
    private Long myEndTimeMillis;
    public synchronized Long getEndTimeMillis(){
        return myEndTimeMillis;
    }

    public synchronized Long getElapsedTimeMillis() {
        if(myStartTimeMillis==null || myEndTimeMillis==null) {
            return null;
        }
        return myEndTimeMillis-myStartTimeMillis;
    }

    private boolean myBatchRequestIsComplete=false;
    public synchronized boolean batchRequestIsComplete() {
        return myBatchRequestIsComplete;
    }
    
    private String myRequestType;
    private String getRequestType() {
        return myRequestType;
    }

    public BatchServiceRequest(INexusService service, String input)  {
        myServices.add(service);
        myInput=input;
        myRequestType="SINGLE";
        myRequests.add(new ServiceRequestThread(service));
    }

    public BatchServiceRequest(List<INexusService> services, String input)  {
        myServices.addAll(services);
        myInput=input;
        myRequestType="MULTIPLE";
        for(INexusService s : myServices){
            if(!s.ignoreBatchRequest())
                { myRequests.add(new ServiceRequestThread(s));}
            else
                { myIgnoredServices.add(s); }
        }
    }

    public synchronized void requestResponses() {
        if(myBatchRequestIsComplete)
            {return;}

        myStartTimeMillis = System.currentTimeMillis();
        for(ServiceRequestThread srt : myRequests){
            srt.requestResponse(myInput);
        }
        
        myReceivedResponses = new ArrayList<GenRespWithConf>();
        myIncompleteRequests = new ArrayList<ServiceRequestThread>(myRequests);
        myCompletedRequests = new ArrayList<ServiceRequestThread>(myRequests);
        while( (System.currentTimeMillis() - myStartTimeMillis) < theMaxWaitTime && !myIncompleteRequests.isEmpty()){
			// Without a sleep call in this loop, it will peg the CPU.
			// Sleep call has to go here so that it happens regardless of "continue"
			try {
				Thread.sleep(200);
			} catch (Throwable t) {
				theLogger.log(Level.WARNING, "Exception during sleep", t);
			}
			List<ServiceRequestThread> recentlyFinished = new ArrayList<ServiceRequestThread>();
            for(ServiceRequestThread srt : myIncompleteRequests){
                if(srt.isAlive())
                    { continue; }
                GenRespWithConf r = srt.getResponse();
                if(r == null)
                    { continue;}

                myReceivedResponses.add(r);
                recentlyFinished.add(srt);
                long elapsed = System.currentTimeMillis() - myStartTimeMillis;
                
                Utils.println("Service[ " + srt.getServiceName() + "] answered in " + elapsed + "msec"
							+ " with confidence " + r.getConfidence());
            }
            myIncompleteRequests.removeAll(recentlyFinished);
            myCompletedRequests.addAll(recentlyFinished);
        }

        if(myReceivedResponses.isEmpty()){ 
            myBestResponse=null;
        }else{
            myBestResponse=Utils.getBest(myReceivedResponses);
        }

        myEndTimeMillis=System.currentTimeMillis();
        Utils.println("Batch completed in " + (System.currentTimeMillis() - myStartTimeMillis + "msec"));
        myBatchRequestIsComplete=true;
    }
    
}
