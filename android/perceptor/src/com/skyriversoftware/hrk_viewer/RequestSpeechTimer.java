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

import android.app.Activity;
import java.util.TimerTask;
import com.skyriversoftware.hrk_viewer.JavaScriptInterface.JsListener;
//import android.content.Context;

public class RequestSpeechTimer extends TimerTask {
	
	Activity mainActivity;
	//Context mContext;
	
	private static JsListener jsListener;
	
	/**
	 * Constructor to instantiate the task and set the context
	 * 
	 * @param context		activity context
	 */
	RequestSpeechTimer(Activity activity) {
		mainActivity = activity;
		//mContext = context;
	}
	
	/**
     * This method is called after expiration of the timer
     */
	@Override
	public void run() {
        if (jsListener == null) { // Get the voiceListener if we haven't already
    		jsListener = (JsListener) mainActivity;
    	}
    	mainActivity.runOnUiThread(new Runnable() {
    		public void run() {
    			jsListener.onVoiceInputRequested(); // If in continuous speech mode, collect speech again!
    		}
    	});
	}
	
}
