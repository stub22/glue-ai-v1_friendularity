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

import java.util.TimerTask;

import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;

/**
 * This is a secondary class for the BudE Proctor skeleton Android tablet application
 * 
 * This TimerTask subclass commands a return to the main Proctor activity, to be run
 * after the expiration of a desired delay 
 * 
 * Last modification 26 March 2012
 * 
 *  * @author Ryan Biggs
 */
public class ReturnToMain extends TimerTask {
	
	Context mContext;
	
	/**
	 * Constructor to instantiate the task and set the context
	 * 
	 * @param context		activity context
	 */
	ReturnToMain(Context context) {
		mContext = context;
	}
	
	/**
     * This method is called after expiration of the timer
     */
	@Override
	public void run() {
		Intent intent = new Intent("android.intent.action.MAIN");
        intent.setComponent(ComponentName.unflattenFromString("com.skyriversoftware.budeproctor/.BudEProctorActivity"));
        intent.addCategory("android.intent.category.LAUNCHER");
        mContext.startActivity(intent);
	}
}
