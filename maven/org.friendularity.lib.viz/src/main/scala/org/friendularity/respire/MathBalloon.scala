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
import org.appdapter.gui.demo.DemoBrowser;

object MathBalloon extends BasicDebugger {
	def main(args: Array[String]) : Unit = {

		// These two lines activate Log4J without requiring a log4j.properties file.  
		// However, when a log4j.properties file is present, these commands should not be used.
		org.apache.log4j.BasicConfigurator.configure();
		org.apache.log4j.Logger.getRootLogger().setLevel(org.apache.log4j.Level.ALL);
		
		val extraDemos = false
		val demoBrowser = false
		if (demoBrowser) {

			DemoBrowser.ensureRunning(true, "no-args");
			DemoBrowser.show();
		}
		if (extraDemos) {
			getLogger().info("^^^^^^^^^^^^^^^^^^^^^^^^  main() starting StructTest.testStructs()");		
			org.friendularity.struct.StructTest.testStructs;

			getLogger().info("^^^^^^^^^^^^^^^^^^^^^^^^  main() starting StructTest.testMathSource()");				
			org.friendularity.struct.StructTest.testClumsyMathSource;

			getLogger().info("^^^^^^^^^^^^^^^^^^^^^^^^  main() starting GridSpaceTest.go");
			// GridSpaceTest 
			org.cogchar.api.space.GridSpaceTest.goGoGo;
		
			getLogger().info("^^^^^^^^^^^^^^^^^^^^^^^^  main() testing math-repo+goody load (respiration)");	
			// This reads in a config model and begins seting up the space, but the individual goodies
			// are not created until render update() callbacks start.
		}
		val sweetDS : Option[SweetDynaSpace] = if (extraDemos) {
			val sds =RespirationTest.initReposLoadMathEval : SweetDynaSpace 
			if (demoBrowser) {
				DemoBrowser.showObject("mathBalloon-sweetDS", sds, true, true); 
				
			}
			Option(sds)
		} else None
		
		getLogger().info("^^^^^^^^^^^^^^^^^^^^^^^^  main() constructing a TrialBalloon OpenGL+MIDI app");
		val bbApp : BigBalloon = new BigBalloon();
		
		if (demoBrowser) {
			DemoBrowser.showObject("mathBalloon-bbApp", bbApp, false, false); // true, true);
		}
		
		getLogger().info("calling tbApp.initMidi()");
		// Initialize available MIDI devices and sequence library.
		bbApp.initMidi();
		getLogger().info("main() calling tbApp (JME3) start(), which will block this thread until done, and will call TrialBalloon.simpleInitApp()");
		// Start the JME3 Virtual world, running all init (i.e. simpleInitApp()) on *this* thread,
		// including blocking waiting for user to say OK to jME launch box.  
		bbApp.start();
		getLogger().info("main() - returned from blocking V-World launch (+ on-thread initApp); we now expect OpenGL VWorld to be running.");
		// Now render thread has started
		if (extraDemos) {
			bbApp.attachDeepDynaSpace(sweetDS.get)		
			//  ...and now sweetDS is getting render-thread callbacks.
			// Thus goodies are being created and displayed on that thread.
		}
		getLogger().info("main() calling tbApp.playMidiOutput()");
		bbApp.playMidiOutput();
		
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
		getLogger().info("^^^^^^^^^^ End of MathBalloon.main()");
		
		
	}
}
import org.cogchar.render.sys.context.CogcharRenderContext;
import org.cogchar.render.sys.registry.RenderRegistryClient;
import com.jme3.scene.{Node}

import org.cogchar.render.goody.dynamic.{DynamicGoodyParent}

// Binds us in to attach a 
class BigBalloon extends TrialBalloon with DynamicGoodyParent {
	
	def attachDeepDynaSpace(sweetDS: SweetDynaSpace) { 
		sweetDS.setParent(this)
		// Attaches the space for callbacks
		attachVWorldUpdater(sweetDS);
	}
	
	override   def getUniqueName() : String = { 
		"generatedName_99";
	}
	
	override def getDisplayNode() : Node = { 	
		val crc  : CogcharRenderContext = getRenderContext();
		val rrc : RenderRegistryClient = crc.getRenderRegistryClient();
		val rootDeepNode = rrc.getJme3RootDeepNode(null)
		rootDeepNode
	}
	
	
}
