/*
 *  Copyright 2013 by The Cogchar Project (www.cogchar.org).
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

package org.friendularity.gmteach.midi;

import org.cogchar.bind.midi.general.FunMidiEventRouter;

/**
 * @author Stu B. <www.texpedient.com>
 */

public class CogcharMidiInputTestMain {
	public static void main(String[] args) {

		FunMidiEventRouter fmer = new FunMidiEventRouter();
		try {
			fmer.startPumpingMidiEvents();
			FunMidiEventRouter.FunListener fl = new FunMidiEventRouter.FunListener();
			fmer.registerListener(fl);
			fmer.logInfo("Showing MIDI events received in next 30 sec...");
			Thread.sleep(30 * 1000);
		} catch (Throwable t) {
			t.printStackTrace();
		} finally {
			fmer.logInfo("Doing cleanup");
			fmer.cleanup();
		}
		fmer.logInfo("main() is done!");
	}	
}