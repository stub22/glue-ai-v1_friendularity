package com.skyriversoftware.hrk_viewer;

import java.io.BufferedReader;
import java.io.InputStream;

import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.client.HttpClient;
//import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.methods.HttpPut;
import org.apache.http.entity.ByteArrayEntity;
//import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.DefaultHttpClient;
//import org.apache.http.message.BasicHeader;
//import org.apache.http.params.BasicHttpParams;
import org.apache.http.params.HttpConnectionParams;
import org.apache.http.util.EntityUtils;
//import org.apache.http.params.HttpParams;
//import org.apache.http.protocol.HTTP;
import org.json.JSONObject;

import android.os.Looper;
import android.util.Log;

import java.io.InputStreamReader;

public class SpeechUtil {
	
	final static int TIMEOUT_MILLISEC = 5000;  // = 5 seconds
	static String requestingId;
	
	// Should really make these methods instance methods and use an instance of SpeechUtil 
	static void postSpeech(final String speechText) {
		Thread t = new Thread(){
			public void run() {
				Log.d("SpeechUtil", "posting speech");
				//Looper.prepare(); //For Preparing Message Pool for the child Thread
                HttpClient client = new DefaultHttpClient();
                HttpConnectionParams.setConnectionTimeout(client.getParams(), TIMEOUT_MILLISEC); //Timeout Limit
                HttpResponse response;
                JSONObject json = new JSONObject();
                try{
                    //HttpPost post = new HttpPost(UrlRegistry.getSpeechUrl());
                	HttpPut put = new HttpPut(UrlRegistry.getSpeechUrl());
                    json.put("speechText", speechText);
                    if (requestingId != null) {
                    	json.put("requestingId", requestingId); // Add the control number that requested the speech to the JSON if it was set
                    }
                    Log.d("SpeechUtil.postSpeech", "Putting: " + json);
                    //StringEntity se = new StringEntity( json.toString());  
                    //se.setContentType(new BasicHeader(HTTP.CONTENT_TYPE, "application/json")); // I the StringEntity approach may work OK too, but untested since I discovered the need for the Accept header
                    //put.setEntity(se);
                    put.setEntity(new ByteArrayEntity(json.toString().getBytes("UTF8")));
                    put.setHeader("Content-Type", "application/json");
                    put.setHeader("Accept", "*/*"); // Doubtful, but maybe this will help? Yes, it's just what we need to get Lift to accept!
                    response = client.execute(put);
                    /*Checking response */
                    if(response!=null) {
                    	HttpEntity responseEntity = response.getEntity();
                    	InputStream in = responseEntity.getContent(); //Get the data in the entity
	                    BufferedReader r = new BufferedReader(new InputStreamReader(in));
	                    StringBuilder total = new StringBuilder();
	                    String line;
	                    while ((line = r.readLine()) != null) {
	                        total.append(line);
	                    }
	                    responseEntity.consumeContent(); // Not sure if this is needed, but trying to figure why delay is required to make text-to-speech work after speech-to-text
	                    Log.d("SpeechUtil.postSpeech", "Response from put: " + total.toString());
                    } else {Log.d("SpeechUtil.postSpeech", "No response from put");}
                    client.getConnectionManager().shutdown();
                }
                catch(Exception e){
                    Log.e("SpeechUtil.postSpeech", "Cannot Estabilish Connection", e);
                } 
                //Looper.loop(); //Loop in the message queue
			}
		};
		t.start();
		/*
		try {
			t.join(5000);
		} catch (Exception e) {
			// Don't need to do anything for Interrupted Exception, I don't think...
		}
		*/
	}
	
	static void setRequestingId(String id) {
		requestingId = id;
	}
}
