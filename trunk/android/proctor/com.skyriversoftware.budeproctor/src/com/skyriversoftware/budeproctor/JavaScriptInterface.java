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

package com.skyriversoftware.budeproctor;

import android.content.Context;
import android.content.Intent;
import android.widget.Toast;
import android.content.ComponentName;
import java.util.Timer;
import static android.content.Intent.*;

/**
 * This is a secondary class for the BudE Proctor skeleton Android tablet application
 * 
 * This class handles JavaScript calls from the BudE Web App to the Proctor Activity
 * 
 * Last modification 26 March 2012
 * 
 *  * @author Ryan Biggs
 */
public class JavaScriptInterface {
    Context mContext;
    Intent intent;

    /* This must be set for the location of the Pushy Web App Server */
    final static String PUSHY_URL = "http://192.168.1.55:8080"; /* Currently set for my test server -  will not work externally */
    
    /**
     * Constructor to instantiate the interface and set the context
     * 
     * @param c			activity context
     */
    JavaScriptInterface(Context c) {
        mContext = c;
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
    	if (tag.equals("BonusDummy1")) {
    		timedLaunch("com.skyriversoftware.budeproctor/.BonusDummy1", delay);
    	}
    	if (tag.equals("Pushy")) {
    		timedUriLaunch(PUSHY_URL, delay);
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
    	ReturnToMain returnTask = new ReturnToMain(mContext);
		new Timer().schedule(returnTask, delay);
		
		/* Launch activity specified */
		if (intent == null) {intent = new Intent("android.intent.action.MAIN");}
        intent.setComponent(ComponentName.unflattenFromString(component));
        intent.addCategory("android.intent.category.LAUNCHER");
        intent.setFlags(FLAG_ACTIVITY_NEW_TASK);	/* This may not have any effect - calls to internal activities stay in same task? */
        mContext.startActivity(intent); 
        intent = null;
    }   
    
    /**
     * Launch web app identified by URI; return after number of seconds specified in delay
     * 
     * @param uri			URI of web component
     * @param delay			delay in seconds before control is returned to Proctor
     */
    void timedUriLaunch(String uri, int delay) {
    	
    	/* Create intent and add extra information to intent to specify desired URI*/
    	intent = new Intent("android.intent.action.MAIN");
    	intent.putExtra("appUri", uri);
    	
    	/* Launch the WebAppBox activity with the specified delay */
    	timedLaunch("com.skyriversoftware.budeproctor/.WebAppBox", delay);
    }   
}
