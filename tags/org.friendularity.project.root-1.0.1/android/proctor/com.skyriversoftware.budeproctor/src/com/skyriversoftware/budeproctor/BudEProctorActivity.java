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

import android.app.Activity;
import android.os.Bundle;
import android.view.View;
import android.webkit.WebSettings;
import android.webkit.WebView;

/**
 * This is the main class for the BudE Proctor skeleton Android tablet application
 * 
 * It is assumed that this activity will run within a kiosk environment such
 * as SureLock to prevent the user from navigating using soft keys
 * 
 * Last modification 26 March 2012
 * 
 *  * @author Ryan Biggs
 */
public class BudEProctorActivity extends Activity {
	
	static WebView proctorWebView;
	
	/**
     * The following method is run by Android upon creation of Proctor activity
     * 
     * @param savedInstanceState	Bundle of View state information used by Android upon restart
     */
    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        
        /* This section prepares to launch the BudE test web page in an Android WebView
         * View class using the layout defined in main.xml
         */
        setContentView(R.layout.main);
        proctorWebView = (WebView) findViewById(R.id.webview);
        WebSettings webSettings = proctorWebView.getSettings();
        webSettings.setJavaScriptEnabled(true);
        proctorWebView.addJavascriptInterface(new JavaScriptInterface(this), "Android");
    }
    
    /**
     * The following method is run by Android when Proctor activity moves to foreground
     */
    @Override
    public void onResume() {
        super.onResume();
        
        /* Load the BudE test web page - this link should be available for testing
         * as of March 2012. It can be changed as desired to use a different page
         * for testing.
         */
        proctorWebView.loadUrl("http://skyriversoftware.com/dev/BudE_test.html");
        
        /* We assume here the system bar should be disabled by kiosk environment
         * Let's "dim" the bar to reduce the distraction (this will not stop
         * soft keys from functioning without a locking kiosk tool).
         */
        proctorWebView.setSystemUiVisibility(View.STATUS_BAR_HIDDEN);
    }    
    
    /**
     * SureLock does not defeat the back button, but that's easy, so we do it here
     * by overriding this method with an empty one.
     */
    @Override
    public void onBackPressed() {
       return;
    }
        
}