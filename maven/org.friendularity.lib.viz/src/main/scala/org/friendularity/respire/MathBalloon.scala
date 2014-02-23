/*
 *  Copyright 2014 by The Friendularity Project (www.friendularity.org).
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

package org.friendularity.respire

import  org.cogchar.render.trial.{TrialBalloon}
import org.appdapter.core.log.BasicDebugger;

object MathBalloon extends BasicDebugger {
	def main(args: Array[String]) : Unit = {

		// These two lines activate Log4J without requiring a log4j.properties file.  
		// However, when a log4j.properties file is present, these commands should not be used.
		org.apache.log4j.BasicConfigurator.configure();
		org.apache.log4j.Logger.getRootLogger().setLevel(org.apache.log4j.Level.INFO);
		val tbApp : TrialBalloon = new TrialBalloon();
		
		getLogger().info("^^^^^^^^^^^^^^^^^^^^^^^^  main() starting GridSpaceTest");
		// GridSpaceTest 
		org.cogchar.api.space.GridSpaceTest.go;
		
		getLogger().info("^^^^^^^^^^^^^^^^^^^^^^^^  main() calling initMidi()");
		// Initialize available MIDI devices and sequence library.
		tbApp.initMidi();
		getLogger().info("^^^^^^^^^^^^^^^^^^^^^^^^  main() calling JME3 start(), which will in turn call TrialBalloon.simpleInitApp()");
		// Start the JME3 Virtual world, running all init (i.e. simpleInitApp()) on *this* thread,
		// including blocking waiting for user to say OK to jME launch box.  
		tbApp.start();

		getLogger().info("^^^^^^^^^^^^^^^^^^^^^^^^  main() - now expects OpenGL VWorld to be running.");
		
		// app.optLoadConfig();
		getLogger().info("^^^^^^^^^^^^^^^^^^^^^^^^ main() calling playMidiOutput()");
		tbApp.playMidiOutput();
		
		// If user escapes out of OpenGL Canvas window while MIDI output still playing in this playMidiOutput method, we get:
		/*
		 * 
124462 [LWJGL Renderer Thread] INFO org.cogchar.render.trial.TrialBalloon  - JME3 destroy() called
124464 [LWJGL Renderer Thread] INFO org.cogchar.render.trial.TrialBalloon  - Cleaning up MIDI bridge
124466 [LWJGL Renderer Thread] INFO org.cogchar.render.trial.TrialBalloon  - MIDI cleanup finished

		 but MIDI thread does not quit. We do eventually get:
		 
143006 [main] INFO org.cogchar.bind.midi.out.CogcharMidiOutputTestMain  - Playing sequence of chords on com.sun.media.sound.RealTimeSequencer@4df93ace
148231 [main] INFO org.friendularity.respire.MathBalloon$  - ^^^^^^^^^^^^^^^^^^^^^^^^ End of main()

		 */
		getLogger().info("^^^^^^^^^^^^^^^^^^^^^^^^ End of main()");
		
		
	}
}
