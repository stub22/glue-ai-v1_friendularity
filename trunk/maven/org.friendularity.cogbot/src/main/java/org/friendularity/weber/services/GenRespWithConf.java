package org.friendularity.weber.services;

// This class contains a response string and an integer confidence level
public class GenRespWithConf
{
    private String myResponse;
    private int myConfidence;

    private String myServiceName;
    public String getServiceName(){
        return myServiceName;
    }

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

    public GenRespWithConf(){
        myResponse = "";
        myConfidence = 0;
    }

    public GenRespWithConf(String response,int confidence) {
        myResponse = response.trim();
		if(myResponse.isEmpty()){
			myConfidence = -1;
		}else{
			myConfidence = confidence;
		}
	}

    public String getResponse() {
        return myResponse;
    }

    public void setResponse(String resp) {
        myResponse = resp;
    }

    public int getConfidence() {
        return myConfidence;
    }

    public void setConfidence(int conf) {
        myConfidence = conf;
    }

    void setServiceUsed(String serviceName) {
        myServiceName=serviceName;
    }

    void setElapsedTimeDetails(long startTime, long endTime) {
        if(startTime>endTime){
            throw new IllegalArgumentException("end time must be >= start time");
        }
        myStartTimeMillis=startTime;
        myEndTimeMillis=endTime;
    }
}