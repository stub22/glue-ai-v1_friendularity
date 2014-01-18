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

package org.friendularity.gmteach.recognizer;

import org.cogchar.bind.midi.general.FunMidiEventRouter;
import org.cogchar.bind.midi.in.InterestingMidiEvent;
import org.cogchar.bind.midi.in.InterestingMidiEvent.ControlChange;
import org.cogchar.bind.midi.in.InterestingMidiEvent.NoteOn;
import org.cogchar.bind.midi.in.MidiEventReporter;
import org.cogchar.bind.midi.out.DemoMidiOutputPlayer;
import org.cogchar.bind.midi.out.NovLpadTest;
import org.cogchar.bind.midi.out.Switcheroo;
import org.friendularity.bundle.demo.gmteach.GMTeachApp;
import org.friendularity.gmteach.estimate.api.west.WorldEstimate;
import org.friendularity.gmteach.goal.GMTeachModule;

/**
 * @author Logicmoo <logicmoo@gmail.com>
 */

public class MidiEventRecognizer extends GMTeachModule implements MidiEventReporter.Listener {

	private FunMidiEventRouter fmer;

	public MidiEventRecognizer(GMTeachApp gmteach) {
		super(gmteach);
	}

	public void init(String[] args) {
		fmer = new FunMidiEventRouter();
		fmer.noPrint = System.err;
		fmer.registerListener(this);
		fmer.startPumpingMidiEvents();
	}

	@Override public void unload() {
		fmer.logInfo("Doing cleanup");
		fmer.cleanup();
		fmer = null;
	}

	public WorldEstimateRecognizer myWERM;
	public FunMidiEventRouter myFMER = new FunMidiEventRouter();
	public NovLpadTest myNLT = new NovLpadTest();
	public DemoMidiOutputPlayer myDMOP = new DemoMidiOutputPlayer();
	public Switcheroo mySwitcheroo;

	@Override public void reportEvent(InterestingMidiEvent ime) {
		try {
			if (myTeach == null) {
				myTeach = GMTeachApp.staticInstance();
			}
			myTeach.reportObject(ime);

			if (mySwitcheroo != null) {
				mySwitcheroo.reportEvent(ime);
			}
			if (myWERM != null) {
				WorldEstimate we = myWERM.getWorldEstimate();
				if (we != null) {
					if (ime instanceof NoteOn) {
						NoteOn noteOn = (NoteOn) ime;
						getLogger().info("Got noteOn: {}", noteOn);
					}
					if (ime instanceof ControlChange) {
						ControlChange cchg = (ControlChange) ime;
						if (((ControlChange) ime).myController == 29) {

						}

						double mult = cchg.myValue / 32.0f;
						getLogger().info("Setting multA to {}", mult + " from " + ime);
						we.mult_A = mult;
					}

				}
			}

		} catch (Throwable t) {
			getLogger().error("Error during midi-mapping", t);
		}
	}

	public void cleanup() {

	}

	public void startMidiRouters(WorldEstimateRecognizer werm) {
		myWERM = werm;
		myFMER.registerListener(this);
		myFMER.startPumpingMidiEvents();
	}

	public void startMidiOutputDemo() {
		int previewMsec = 2000;
		myDMOP.playAllDemoSeqsBriefly_Blocking(previewMsec); // Currently leaves the last one still playing
	}

	public void startMidiSwitcherooDemo() {
		mySwitcheroo = new Switcheroo();
		mySwitcheroo.myDMOP = myDMOP;
		boolean lpadOK = myNLT.startLightDemo();
		if (lpadOK) {
			mySwitcheroo.myNLT = myNLT;
		}
	}
}
