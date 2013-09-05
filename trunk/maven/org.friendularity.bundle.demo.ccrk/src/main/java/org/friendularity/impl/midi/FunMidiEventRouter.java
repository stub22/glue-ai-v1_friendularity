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


import java.io.OutputStream;
import java.io.PrintStream;
import javax.sound.midi.MidiDevice;
import javax.sound.midi.MidiMessage;
import javax.sound.midi.MidiSystem;
import javax.sound.midi.MidiUnavailableException;
import javax.sound.midi.Receiver;
import javax.sound.midi.Sequence;
import javax.sound.midi.Transmitter;
import org.appdapter.core.log.BasicDebugger;

/**
 * @author Stu B. <www.texpedient.com>
 */
public class FunMidiEventRouter extends BasicDebugger {

	private		MidiDevice		myDevice = null;
	private		DumpReceiver	myReceiver = null;
	public FunMidiEventRouter() { 
		// OutputStream noOut = new NullOutputStream();
		PrintStream noPrint = null; // new PrintStream(noOut);
		myReceiver = new DumpReceiver(noPrint); // System.out);
	}
	public void registerListener(MidiEventReporter.Listener listener) {
		myReceiver.registerListener(listener);
	}		
	public void startPumpingMidiEvents() {
		try {
			MidiDevice.Info inputInfo = fetchInputDeviceInfo();
			myDevice = MidiSystem.getMidiDevice(inputInfo);
			if (!(myDevice.isOpen())) {
				myDevice.open();
			}
			Transmitter transmitter = myDevice.getTransmitter();

			transmitter.setReceiver(myReceiver);

		} catch (Throwable t) {
			getLogger().error("Caught: ", t);
			cleanup();
		}
	}


	public void cleanup() {
		try {
			if (myDevice != null) {
				myDevice.close();
			}
		} catch (Throwable t) {
			getLogger().error("Caught: ", t);
		}
	}
	public static class FunListener extends BasicDebugger implements MidiEventReporter.Listener {

		@Override public void reportEvent(InterestingMidiEvent ime) {
			getLogger().info("Received interesting midi event: {} ", ime);
		}
		
	}
	static class OurMidiReceiver extends BasicDebugger implements Receiver {

		@Override public void send(MidiMessage message, long timeStamp) {
			getLogger().info("Received at " + timeStamp + ": " + message);
		}

		@Override public void close() {
			getLogger().info("Closing");
		}
	}

	public MidiDevice.Info fetchInputDeviceInfo() throws Throwable {
		MidiDevice.Info devInfoArr[] = MidiSystem.getMidiDeviceInfo();
		if (devInfoArr == null) {
			return null;
		}
		getLogger().info("DeviceInfo Array has length: " + devInfoArr.length);
		for (int i = 0; i < devInfoArr.length; i++) {
			getLogger().info("-----------------------------");
			MidiDevice.Info devInfo = devInfoArr[i];
			getLogger().info("Checking:  " + devInfo.toString());
			Transmitter tmit = fetchTransmitter(devInfo);
			if (tmit != null) {
				String name = devInfo.getName();
				getLogger().info("FOUND TRANSMITTER: " + tmit);
				MidiDevice device = MidiSystem.getMidiDevice(devInfo);
				getLogger().info("ON DEVICE: " + device);
				getLogger().info("Vendor: " + devInfo.getVendor());
				getLogger().info("Name: " + name);
				getLogger().info("Description: " + devInfo.getDescription());
				if (name.equals("USB Audio Device") || name.equals("MPK mini")) {
					getLogger().info("MATCH\n===============");
					return devInfo;
				}
			}
			//if (device instanceof Transmitter) {
			//	System.out.println("Version: " + info.getVersion());
			//	if (info.getDescription().equals("External MIDI Port")) {
			//		return info;
			//	}
		}
		return null;
	}

	public Transmitter fetchTransmitter(MidiDevice.Info info) {
		try {
			MidiDevice device = MidiSystem.getMidiDevice(info);
			getLogger().info("Got device: " + device);
			Transmitter t = device.getTransmitter();
			return t;
		} catch (Throwable t) {
			getLogger().warn("Cannot fetch transmitter for {}", info);
		}
		return null;

	}

	public static void main(String[] args) {

		FunMidiEventRouter fmer = new FunMidiEventRouter();
		try {
			fmer.startPumpingMidiEvents();
			FunListener fl = new FunListener();
			fmer.registerListener(fl);			
			Thread.sleep(30 * 1000);
		} catch (Throwable t) {
			t.printStackTrace();
		} finally {
			fmer.cleanup();
		}
	}
}
