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
package org.friendularity.migccmio;

import org.appdapter.core.log.BasicDebugger;
import org.cogchar.bind.midi.general.FunMidiEventRouter;
import org.cogchar.bind.midi.in.CCParamRouter;
import org.cogchar.bind.midi.in.InterestingMidiEvent;
import org.cogchar.bind.midi.in.InterestingMidiEvent.ControlChange;
import org.cogchar.bind.midi.in.InterestingMidiEvent.NoteOn;
import org.cogchar.bind.midi.in.MidiEventReporter;
import org.cogchar.bind.midi.out.DemoMidiOutputPlayer;
import org.cogchar.bind.midi.out.NovLpadTest;
import org.cogchar.bind.midi.out.Switcheroo;
import org.friendularity.api.west.WorldEstimate;

/**
 * @author Stu B. <www.texpedient.com>
 */
public class Mig_DemoMidiCommandMapper extends BasicDebugger implements MidiEventReporter.Listener {

	public Mig_WorldEstimateRenderModule myWERM;
	public	FunMidiEventRouter			myFMER = new FunMidiEventRouter();
	public	NovLpadTest					myNLT  = new NovLpadTest();
	public	DemoMidiOutputPlayer		myDMOP  = new DemoMidiOutputPlayer();
	public	Switcheroo					mySwitcheroo;
	public	CCParamRouter				myCCPR;
	
	@Override public void reportEvent(InterestingMidiEvent ime) {
		try {
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
	public void cleanup() { 
		
	}
	protected void startMidiRouters() { 
		
		myFMER.registerListener(this);		
		myFMER.startPumpingMidiEvents();		
		myCCPR = new CCParamRouter(myFMER);
	}	
	public void setWERM(Mig_WorldEstimateRenderModule werm) {
		myWERM = werm;
	}
	protected void startMidiOutputDemo() { 
		int previewMsec = 2000;
		getLogger().info("Temporarily skipping MIDI output demo, which needs to be started asynchronously instead!");
		// myDMOP.playAllDemoSeqsBriefly_Blocking(previewMsec);  // Currently leaves the last one still playing
	}
	protected void startMidiSwitcherooDemo() { 
		mySwitcheroo = new Switcheroo();
		mySwitcheroo.myDMOP = myDMOP;
		boolean lpadOK = myNLT.startLightDemo();
		if (lpadOK) {
			mySwitcheroo.myNLT = myNLT;
		}
	}
	public static class MidiLaunchWrapper {
		private boolean myFlag_connectMidiIn = true;
		private boolean myFlag_connectMidiOut = true;
		private boolean myFlag_connectMidiSwitcheroo = true;

		private Mig_DemoMidiCommandMapper myMidiMapper;
		public Mig_DemoMidiCommandMapper initMapperWithFeatures() {
			if (myMidiMapper == null) {
				myMidiMapper = new Mig_DemoMidiCommandMapper();

				if (myFlag_connectMidiIn) {
					myMidiMapper.startMidiRouters();
				}
				if (myFlag_connectMidiOut) {
					myMidiMapper.startMidiOutputDemo();
				}
				if (myFlag_connectMidiSwitcheroo) {
					myMidiMapper.startMidiSwitcherooDemo();
				}
			}

			return myMidiMapper;
		}
	}
}