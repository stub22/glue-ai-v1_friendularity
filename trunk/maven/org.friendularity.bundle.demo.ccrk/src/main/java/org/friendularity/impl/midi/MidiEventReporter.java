/*
 *  Copyright 2013 by The Friendularity Project (www.friendularity.org).
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

package org.friendularity.impl.midi;

import java.util.ArrayList;
import java.util.List;

/**
 * @author Stu B. <www.texpedient.com>
 */

public class MidiEventReporter {
	public static interface Listener {
		public void reportEvent(InterestingMidiEvent ime);
	}
	private List<Listener> myListeners = new ArrayList<Listener>();
	public void registerListener(Listener l) {
		myListeners.add(l);
	}
	protected void deliverEvent(InterestingMidiEvent ime) { 
		for (Listener l : myListeners) {
			l.reportEvent(ime);
		}
	}
	protected void noticeControlChange(int channel, int controller, int value) {
		InterestingMidiEvent ime = new InterestingMidiEvent.ControlChange(channel, controller, value);
		deliverEvent(ime);
	}
	protected void noticeNoteOn(int channel, int note, int vel) {
		InterestingMidiEvent ime = new InterestingMidiEvent.NoteOn(channel, note, vel);
		deliverEvent(ime);
	}
	protected void noticeNoteOff(int channel, int note, int vel) {
		InterestingMidiEvent ime = new InterestingMidiEvent.NoteOff(channel, note, vel);
		deliverEvent(ime);
	}	
}
