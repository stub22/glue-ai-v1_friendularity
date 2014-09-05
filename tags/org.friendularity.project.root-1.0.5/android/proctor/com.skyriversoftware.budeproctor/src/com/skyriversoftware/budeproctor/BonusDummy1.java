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

/**
 * This is the main class for a BudE "bonus" dummy application
 * 
 * It is assumed that this activity will run within a kiosk environment such
 * as SureLock to prevent the user from navigating using soft keys
 * 
 * Last modification 26 March 2012
 * 
 *  * @author Ryan Biggs
 */
public class BonusDummy1 extends Activity {

	/**
     * The following method is run by Android upon creation of activity
     * 
     * @param savedInstanceState	Bundle of View state information used by Android upon restart
     */
	public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.bonusdummy1_layout); /* Use this layout for the activity */
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
