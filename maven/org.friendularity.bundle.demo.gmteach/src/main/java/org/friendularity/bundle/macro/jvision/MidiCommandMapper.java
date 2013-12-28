/*
 *  Copyright 2013 by The Friendularity Project (www.friendularity.gmteach.org).
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
package org.friendularity.bundle.macro.jvision;

import org.appdapter.core.log.BasicDebugger;
import org.cogchar.bind.midi.in.InterestingMidiEvent;
import org.cogchar.bind.midi.InterestingMidiEvent.ControlChange;
import org.cogchar.bind.midi.InterestingMidiEvent.NoteOn;
import org.cogchar.bind.midi.in.MidiEventReporter;
import org.friendularity.gmteach.api.west.WorldEstimate;
import org.friendularity.gmteach.impl.visual.WorldEstimateRenderModule;

/**
 * @author Stu B. <www.texpedient.com>
 */
public class MidiCommandMapper extends BasicDebugger implements MidiEventReporter.Listener {

	public WorldEstimateRenderModule myWERM;

	@Override public void reportEvent(InterestingMidiEvent ime) {
		try {
			if (myWERM != null) {
				WorldEstimate we = myWERM.getWorldEstimate();
				if (we != null) {
					if (ime instanceof NoteOn) {
						NoteOn noteOn = (NoteOn) ime;
					}
					if (ime instanceof ControlChange) {
						ControlChange cchg = (ControlChange) ime;
						double mult = cchg.myValue / 32.0f;
						getLogger().debug("Setting multA to {}", mult);
						we.mult_A = mult;
					}
				}
			}
			
		} catch (Throwable t) {
			getLogger().error("Error during midi-mapping", t);
		}

	}
}
