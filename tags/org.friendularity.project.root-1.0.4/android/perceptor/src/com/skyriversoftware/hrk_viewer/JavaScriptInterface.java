/*
 *  Copyright 2012 by Sky River Software (skyriversoftware.com).
 * 
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 * 
 *       http://www.apache.org/licenses/LICENSE-2.0
 * 
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package com.skyriversoftware.hrk_viewer;

import static android.content.Intent.*;

import java.util.Date;
import java.util.Timer;

import android.app.Activity;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.res.Resources;
import android.util.Log;
import android.widget.Toast;

/**
 * This is a secondary class for the BudE Proctor skeleton Android tablet application
 * 
 * This class handles JavaScript calls from the BudE Web App to the Proctor Activity
 * 
 * Last modification 21 June 2012 for hrk_viewer fork
 * 
 *  * @author Ryan Biggs
 */
public class JavaScriptInterface {
    static Activity mainActivity;
    static Context mContext;
    Intent intent; 
    
    private Resources res;
    
    private static JsListener jsListener;
    
    //private static int runCounter = 0; // FOR TEST ONLY
    private static long lastSpeechTime;
    private static long lastSpeechOutTime;
    
    // Container Activity must implement this interface: JavaScript Listener currently used for Voice Input/Output
    public interface JsListener {
        public void onVoiceInputRequested();
        public void onVoiceOutputRequested(String text);
        public void startContinuousSpeechInput();
        public void stopContinuousSpeechInput();
    }
    
    /**
     * Constructor to instantiate the interface and set the context
     * 
     * @param c			activity context
     */
    JavaScriptInterface(Activity app) {
        mainActivity = app;
        mContext = (Context)app;
        res = mainActivity.getResources();
    }	

    /**
     * This method shows a Toast pop-up as a simple demonstration
     * 
     * @param toast		the string to be displayed
     */
    public void showToast(String toast) {
        Toast.makeText(mContext, toast, Toast.LENGTH_SHORT).show();
    }
    
    /**
     * This method starts other Android activities as specified by the
     * JavaScript call, then returns to main Proctor activity after the
     * specified delay
     * 
     * Currently the following two activities are implemented:
     * "BonusDummy1"	Dummy "bonus" activity
     * "Pushy" 			Friendularity "Pushy" Web App 
     * 
     * @param tag		string used to specify desired activity
     * @param delay		delay in seconds before control is returned to Proctor
     */
    public void startActivity(String tag, int delay) {
    	if (tag.equals("Pushy")) {
    		timedUriLaunch(UrlRegistry.getPushyUrl(), delay);
    	}
    	
    	/* Additional activities may be added here */
    }
    
    /**
     * Launch component identified by string; return after number of seconds specified in delay
     * 
     * @param component		name of Android component
     * @param delay			delay in seconds before control is returned to Proctor
     */
    void timedLaunch(String component, int delay) {
    	delay = delay * 1000;	/* Convert time to milliseconds */
    	
    	/* Set up timed return to main activity */
    	ReturnToMainActivity returnTask = new ReturnToMainActivity(mContext);
		new Timer().schedule(returnTask, delay);
		
		/* Launch activity specified */
		if (intent == null) {intent = new Intent("android.intent.action.MAIN");}
        intent.setComponent(ComponentName.unflattenFromString(component));
        //intent.addCategory("android.intent.category.LAUNCHER");
        //intent.setFlags(FLAG_ACTIVITY_NEW_TASK);	/* This may not have any effect - calls to internal activities stay in same task? */
        //intent.setFlags(FLAG_ACTIVITY_REORDER_TO_FRONT);
        mContext.startActivity(intent); 
        intent = null;
        //WebAppBox appBox = (WebAppBox)mContext;
        //appBox.finish(); // We force it to close. Will new instance once we return work OK?
    }   
    
    /**
     * Launch web app identified by URI; return after number of seconds specified in delay
     * 
     * @param uri			URI of web component
     * @param delay			delay in seconds before control is returned to Proctor
     */
    void timedUriLaunch(String uri, int delay) {
    	
    	// Let's not do it this way
    	/* 
    	// Create intent and add extra information to intent to specify desired URI
    	intent = new Intent("android.intent.action.MAIN");
    	intent.putExtra("appUri", uri);
    	
    	// Launch the WebAppBox activity with the specified delay 
    	timedLaunch("com.skyriversoftware.budeproctor/.WebAppBox", delay);
    	*/
    	
    	// ... Making a new intent for WebAppBox is confusing Speech activity intent callbacks, so let's just update the WebAppBox directly
    	WebAppBox appBox = (WebAppBox)mContext;
    	appBox.setUrl(uri);
    	ReturnToMainUrl returnTask = new ReturnToMainUrl(mContext);
    	delay = delay * 1000;
		new Timer().schedule(returnTask, delay);
    }
    
    // Called by Lift to get a single speech input
    public boolean getSpeechInput() { 
    	boolean success = false;
    	if (isUniqueSpeechRequest()) {
    		getOneSpeechInput();
        	success = true;
    	} 
    	//Log.d("JavaScriptInterface", "getSpeechInput called " + runCounter + " times"); // TEST ONLY
    	return success;
    }
    
    public void getSpeechInput(String id) {
    	/* This was a kind of screwy thing I was doing to try to support very old Friendularity lifters - I think I was "sneaking" requesting ID into SpeechUtil before speech results were returned
    	if (getSpeechInput()) {
    		SpeechUtil.setRequestingId(id);
    	}
    	*/
    	Log.i("JavaScriptInterface", "Getting speech input for id " + id);
    	SpeechUtil.setRequestingId(id);
    	getSpeechInput();
    }
    
    public void getContinuousSpeechInput(String id) {
    	if (isUniqueSpeechRequest()) {
			SpeechUtil.setRequestingId(id);
			Log.d("JavaScriptInterface.getContinuousSpeechInput", "Getting \"continuous\" speech from Google now.");
	    	if (jsListener == null) { // Get the voiceListener if we haven't already
	    		jsListener = (JsListener) mContext;
	    	}
	    	mainActivity.runOnUiThread(new Runnable() {
	    		public void run() {
	    			jsListener.startContinuousSpeechInput();
	    		}
	    	});
    	}
    }
    
    public void stopContinuousSpeechInput() {
    	Log.d("JavaScriptInterface.stopContinuousSpeechInput" , "Continuous Speech Input Stop Requested");
    	if (jsListener != null) {
    		Log.d("JavaScriptInterface.stopContinuousSpeechInput" , "jsListener found, stopping...");
    		jsListener.stopContinuousSpeechInput();
    	}
    }
    
    // Requests recognition of a single phrase - not to be called directly by Lift
    private void getOneSpeechInput() {
    	Log.d("JavaScriptInterface.getOneSpeechInput", "Getting speech from Google now.");
    	if (jsListener == null) { // Get the voiceListener if we haven't already
    		jsListener = (JsListener) mContext;
    	}
    	//jsListener.onVoiceInputRequested(); // Needs to be run on main thread; otherwise runs on WebViewMainThread because JS does
    	mainActivity.runOnUiThread(new Runnable() {
    		public void run() {
    			jsListener.onVoiceInputRequested();
    		}
    	});
    	//SpeechUtil.setRequestingId(null); // Blanks the requesting ID - ensures that SpeechUtil sends JSON without ID if "original" method only is called
    }
    
    // In some cases, currently Lift may call this multiple times for a single request
	// Band-aid: ignore any additional requests within 5 seconds (as set in XML).
    // Return true if this is not a duplicate request
    private boolean isUniqueSpeechRequest() {
    	boolean unique = false;
    	Date javaDate = new Date();
    	long thisReqTime = javaDate.getTime();
    	long lastReqTime = lastSpeechTime;

    	Log.d("JavaScriptInterface.getSpeechInput", "Speech requested by JavaScript at " + thisReqTime + ", last at " + lastReqTime + "; ignore time is " + res.getInteger(R.integer.duplicateSpeechRequestIgnoreTime));
    	if ((thisReqTime - lastReqTime) > Long.valueOf(res.getInteger(R.integer.duplicateSpeechRequestIgnoreTime))) {
    		unique = true;
    		lastSpeechTime = thisReqTime;
    	} else {
    		Log.d("JavaScriptInterface.checkUniqueSpeechRequest", "Speech was just requested " + (thisReqTime - lastReqTime) + " milliseconds ago, silly Lift. Ignoring.");
    	}
    	return unique;
    }
    
    public void outputSpeech(final String text) {
    	// Same Band-aid for now: ignore any additional requests within 5 seconds.
    	Date javaDate = new Date();
    	long thisReqTime = javaDate.getTime();
    	long lastReqTime = lastSpeechOutTime;

    	Log.d("JavaScriptInterface.outputSpeech", "Speech output requested by JavaScript at " + thisReqTime + ", last at " + lastReqTime);
    	if ((thisReqTime - lastReqTime) > 5000) {
        	lastSpeechOutTime = thisReqTime;
        	Log.d("JavaScriptInterface.outputSpeech", "Speaking now.");
        	if (jsListener == null) { // Get the voiceListener if we haven't already
        		jsListener = (JsListener) mContext;
        	}
        	mainActivity.runOnUiThread(new Runnable() {
        		public void run() {
        			jsListener.onVoiceOutputRequested(text); // May not really matter if this is on main thread or not
        		}
        	});
    	} else {
    		Log.d("JavaScriptInterface.getSpeechInput", "Speech out was just requested " + (thisReqTime - lastReqTime) + " milliseconds ago, silly Lift. Ignoring.");
    	}
    }
    	
}
