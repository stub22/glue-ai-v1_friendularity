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

/**
 * @author Stu B. <www.texpedient.com>
 */

public class InterestingMidiEvent {

	public int	myChannel;
	
	
	public static class ControlChange extends InterestingMidiEvent {
		public int	myController;
		public int	myValue;
		
		public ControlChange(int channel, int controller, int value) {
			myChannel = channel;
			myController = controller;
			myValue = value;
		}
		public String toString() {
			return "ControlChange[chan=" + myChannel + ", ctrl=" + myController + ", val=" + myValue + "]";
		}
	}
	public static abstract class Note extends InterestingMidiEvent {
		public int myNote;
		public int myVelocity;
		public Note(int channel, int note, int vel) {
			myChannel = channel; 
			myNote = note;
			myVelocity = vel;
		}
	}
	public static class NoteOn  extends Note  {
		public NoteOn(int channel, int note, int vel) {
			super(channel, note, vel);
		}
		public String toString() {
			return "NoteOn[chan=" + myChannel + ", note=" + myNote + ", vel=" + myVelocity + "]";
		}		
	}
	public static class NoteOff  extends Note {
		public NoteOff(int channel, int note, int vel) {
			super(channel, note, vel);
		}
		public String toString() {
			return "NoteOff[chan=" + myChannel + ", note=" + myNote + ", vel=" + myVelocity + "]";
		}		
	}
}
