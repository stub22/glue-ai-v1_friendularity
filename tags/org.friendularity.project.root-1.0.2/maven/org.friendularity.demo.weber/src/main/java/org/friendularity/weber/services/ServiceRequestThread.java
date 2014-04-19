/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.friendularity.weber.services;

/**
 *
 * @author matt
 */
public class ServiceRequestThread implements Runnable {
    private GenRespWithConf myResponse;
    private INexusService myService;
    private String myInput;
    private Thread myThread;

    private Long myStartTimeMillis=null;
    public Long getStartTimeMillis() {
        return myStartTimeMillis;
    }

    private Long myEndTimeMillis=null;
    public Long getEndTimeMillis() {
        return myEndTimeMillis;
    }

    public Long getElapsedTimeMillis() {
        if(myStartTimeMillis==null || myEndTimeMillis==null) {
            return null;
        }
        return myEndTimeMillis-myStartTimeMillis;
    }

    public ServiceRequestThread(INexusService service){
        myService = service;
    }

    public void requestResponse(String input){
        if(myThread!=null){
            return;
        }

        myInput = input;
        myThread = new Thread(this);
        myThread.start();
    }

    public void run() {
        myResponse = null;

        myStartTimeMillis=System.currentTimeMillis();
        GenRespWithConf res = myService.getResponse(myInput);
        myEndTimeMillis=System.currentTimeMillis();

        if(res != null){
            res.setServiceUsed( myService.getServiceName() );
            res.setElapsedTimeDetails(myStartTimeMillis,myEndTimeMillis);
            myResponse = res;
        }
    }

    public GenRespWithConf getResponse(){
        if(myThread.isAlive())
            return null;
        return myResponse;
    }
          
    public boolean isAlive(){
        return myThread.isAlive();
    }

    public String getServiceName(){
        return myService.getServiceName();
    }

    public INexusService getService(){
        return myService;
    }
}
