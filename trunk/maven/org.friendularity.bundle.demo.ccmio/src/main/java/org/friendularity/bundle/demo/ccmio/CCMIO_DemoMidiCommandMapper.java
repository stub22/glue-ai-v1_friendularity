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
package org.friendularity.bundle.demo.ccmio;

import org.appdapter.core.log.BasicDebugger;
import org.friendularity.api.west.WorldEstimate;
import org.friendularity.impl.visual.WorldEstimateRenderModule;
import org.cogchar.bind.midi.in.InterestingMidiEvent;
import org.cogchar.bind.midi.in.InterestingMidiEvent.NoteOn;
import org.cogchar.bind.midi.in.InterestingMidiEvent.ControlChange;
import org.cogchar.bind.midi.in.MidiEventReporter;
import org.cogchar.bind.midi.general.FunMidiEventRouter;

import org.cogchar.bind.midi.out.DemoMidiOutputPlayer;
import org.cogchar.bind.midi.out.NovLpadTest;
import org.cogchar.bind.midi.out.Switcheroo;
import org.cogchar.bind.midi.in.CCParamRouter;
/**
 * @author Stu B. <www.texpedient.com>
 */
public class CCMIO_DemoMidiCommandMapper extends BasicDebugger implements MidiEventReporter.Listener {

	public	WorldEstimateRenderModule	myWERM;
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
	public void setWERM(WorldEstimateRenderModule werm) { 
		myWERM = werm;
	}
	protected void startMidiOutputDemo() { 
		int previewMsec = 2000;
		myDMOP.playAllDemoSeqsBriefly_Blocking(previewMsec);  // Currently leaves the last one still playing
	}
	protected void startMidiSwitcherooDemo() { 
		mySwitcheroo = new Switcheroo();
		mySwitcheroo.myDMOP = myDMOP;
		boolean lpadOK = myNLT.startLightDemo();
		if (lpadOK) {
			mySwitcheroo.myNLT = myNLT;
		}
	}	
}
