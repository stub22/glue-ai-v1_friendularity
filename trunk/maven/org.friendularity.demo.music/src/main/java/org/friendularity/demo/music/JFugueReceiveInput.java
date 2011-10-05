/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.friendularity.demo.music;

import javax.sound.midi.MidiDevice;
import javax.sound.midi.MidiSystem;
import javax.sound.midi.Sequence;
import javax.sound.midi.Transmitter;
import org.jfugue.DeviceThatWillTransmitMidi;
import org.jfugue.PatternInterface;
import org.jfugue.Player;

/**
 *
 * @author Stu Baurmann
 */
public class JFugueReceiveInput {

	public static void main(String[] args) {
		try {
			MidiDevice.Info  inputInfo = getInputDeviceInfo();
			Sequence seq = new Sequence(Sequence.PPQ, 10);
			DeviceThatWillTransmitMidi device = new DeviceThatWillTransmitMidi(inputInfo);
			System.out.println("Got device: " + device);
			device.listenForMillis(5000);
			System.out.println("Finished listening");
			// Sequence music = device.getSequenceFromListening();
			// System.out.println("Sequence: " + music);
			PatternInterface pat = device.getPatternFromListening();
			System.out.println("Pattern: " + pat);
			Player player = new Player();
			player.play(pat);
		} catch (Throwable t) {
			t.printStackTrace();
		}
	}

	public static MidiDevice.Info getInputDeviceInfo() throws Throwable {
		MidiDevice.Info infoArr[] = MidiSystem.getMidiDeviceInfo();

		for (int i = 0; i < infoArr.length; i++) {
			System.out.println("-----------------------------");
			MidiDevice.Info info = infoArr[i];
			System.out.println("Checking:  " + info.toString());
			Transmitter t = getTransmitter(info);
			if (t != null) {
				System.out.println("FOUND TRANSMITTER: " + t);
				MidiDevice device = MidiSystem.getMidiDevice(info);
				System.out.println("ON DEVICE: " + device);
				System.out.println("Vendor: " + info.getVendor());
				System.out.println("Name: " + info.getName());
				System.out.println("Description: " + info.getDescription());
				if (info.getName().equals("USB Audio Device")) {
					System.out.println("MATCH\n===============");
					return info;
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
	public static Transmitter getTransmitter(MidiDevice.Info info) {
		try {
			MidiDevice device = MidiSystem.getMidiDevice(info);
			System.out.println("Got device: " + device);
			Transmitter t = device.getTransmitter();
			return t;
		} catch (Throwable t) {
			System.out.println("Error fetching transmitter: " + t);
		}
		return null;
	
	}
}
