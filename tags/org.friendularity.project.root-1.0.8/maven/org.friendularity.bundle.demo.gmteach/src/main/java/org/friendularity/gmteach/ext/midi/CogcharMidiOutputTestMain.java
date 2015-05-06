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
package org.friendularity.gmteach.ext.midi;

import java.util.ArrayList;
import java.util.List;

import javax.sound.midi.Instrument;
import javax.sound.midi.MidiChannel;
import javax.sound.midi.MidiDevice;
import javax.sound.midi.MidiSystem;
import javax.sound.midi.Patch;
import javax.sound.midi.Receiver;
import javax.sound.midi.Sequence;
import javax.sound.midi.Sequencer;
import javax.sound.midi.ShortMessage;
import javax.sound.midi.Soundbank;
import javax.sound.midi.Synthesizer;
import javax.sound.midi.Transmitter;
import javax.sound.midi.VoiceStatus;

import org.appdapter.core.log.BasicDebugger;
import org.cogchar.bind.midi.general.FunMidiEventRouter;
import org.cogchar.bind.midi.general.MidiDevMatchPattern;
import org.cogchar.bind.midi.general.MidiDevWrap;
import org.cogchar.bind.midi.out.MidiTrackFactory;
import org.cogchar.bind.midi.out.NovLpadTest;
import org.cogchar.bind.midi.seq.DemoMidiSeq;

/**
 * @author Stu B. <www.texpedient.com>
 * 
 *         Some of the code and comments in this file are copied/adapted from the JSResources.org code + docs.
 */
public class CogcharMidiOutputTestMain extends BasicDebugger {

	public static void main(String[] args) {
		// Because there is a log4j.properties file in this project, we do not need to do this Log4J config from code.
		// It seems that when we do, we get two sets of loggers, which is kinda gross looking in the console output.
		// But in another main, somewhere else, we might want these two lines uncommented to make logging output go.
		// org.apache.log4j.BasicConfigurator.configure();
		// org.apache.log4j.Logger.getRootLogger().setLevel(org.apache.log4j.Level.ALL);

		FunMidiEventRouter fmer = new FunMidiEventRouter();
		try {
			CogcharMidiOutputTestMain cmotm = new CogcharMidiOutputTestMain();

			NovLpadTest nlt = new NovLpadTest();
			nlt.startLightDemo();

			cmotm.testMPMP(); // Does not currently wait for seq to finish
			// ...so it is playing now, and will overlap with anything done next.
			// 
			// cmotm.playSomeNotes();
		} catch (Throwable t) {
			t.printStackTrace();
		} finally {
			fmer.logInfo("Doing cleanup");
			fmer.cleanup();
		}
		fmer.logInfo("main() is done!");
	}

	public void findNocturnOutput() throws Throwable {
		MidiDevMatchPattern devPattern = new MidiDevMatchPattern();
	}

	public void playSomeNotes() throws Throwable {
		MidiDevMatchPattern devPattern = new MidiDevMatchPattern();

		List<MidiDevWrap> devs = MidiDevWrap.findMatchingDevs(devPattern, getLogger());

		for (MidiDevWrap devWrap : devs) {
			MidiDevice dev = devWrap.myDevice;
			if (dev instanceof Synthesizer) {
				getLogger().info("Found synthesizer {} of class {}", dev, dev.getClass());
			}
			if (dev instanceof Sequencer) {
				getLogger().info("Found sequencer {} of class {}", dev, dev.getClass());
			}

		}
		// getSequencer()
		// Obtains the default Sequencer, connected to a default device.
		/*
				 * public static Sequencer getSequencer(boolean connected) throws MidiUnavailableException
				 * Obtains the default Sequencer, optionally connected to a default device.
				 * If connected is true, the returned Sequencer instance is connected to the default Synthesizer, as returned by 
				 * MidiSystem.getSynthesizer. If there is no Synthesizer available, or the default Synthesizer cannot be opened, the 
				 * sequencer is connected to the default Receiver, as returned by MidiSystem.getReceiver. The connection is made by 
				 * retrieving a Transmitter instance from the Sequencer and setting its Receiver. Closing and re-opening the sequencer 
				 * will restore the connection to the default device.
				 * If connected is false, the returned Sequencer instance is not connected, it has no open Transmitters. 
				 * In order to play the sequencer on a MIDI device, or a Synthesizer, it is necessary to get a Transmitter and set its 
				 * Receiver.  If the system property javax.sound.midi.Sequencer is defined or it is defined in the file 
				 * "sound.properties", it is used to identify the default sequencer. For details, refer to the class description.
				 */
		boolean doConnectSeqToSynth = true;
		Sequencer dseq = MidiSystem.getSequencer(doConnectSeqToSynth);
		getLogger().info("System default sequencer is {}, of class {}", dseq, (dseq != null) ? dseq.getClass() : "NULL");
		if (dseq != null) {
			playChordSequence(dseq);
		}
		Synthesizer dsynth = MidiSystem.getSynthesizer();
		getLogger().info("System default synthesizer is {}, of class {}", dsynth, (dsynth != null) ? dsynth.getClass() : "NULL");
		if (dsynth != null) {
			playSoundbankDemo(dsynth);
		}
		if (dseq != null) {
			playChordSequence(dseq);
		}
	}

	private void playChordSequence(Sequencer dseq) throws Throwable {
		getLogger().info("Playing sequence of chords on {}", dseq);
		if (dseq != null) {
			Sequence seq = MidiTrackFactory.makeSequenceOfChords();
			dseq.open();
			dseq.setSequence(seq);
			dseq.start();
			Thread.sleep(5000);
			dseq.close();
		}
	}

	private void printChanFacts(MidiChannel chan) {
		getLogger().info("program={}", chan.getProgram());

		getLogger().info("mono={}", chan.getMono()); // as in mono/poly-phonic
		getLogger().info("solo={}", chan.getSolo());
		getLogger().info("mute={}", chan.getMute());
		getLogger().info("omni={}", chan.getOmni()); // In omni mode, the channel responds to messages sent on all channels. 
		getLogger().info("pitchBend={}", chan.getPitchBend());
		getLogger().info("channelPressure={}", chan.getChannelPressure());

		int noteNum = 25;
		getLogger().info("polyPressure(note={})={}", noteNum, chan.getPolyPressure(noteNum));

		int controllerNum = 20;
		getLogger().info("controllerVal(controlNum={})={}", controllerNum, chan.getController(controllerNum));
	}

	private void playSoundbankDemo(Synthesizer dsynth) throws Throwable {

		getLogger().info("Synth latency={}, polyphony={}", dsynth.getLatency(), dsynth.getMaxPolyphony());
		Soundbank dsdbk = dsynth.getDefaultSoundbank();
		getLogger().info("Default synth has default soundbank {}, of class {}", dsdbk, (dsdbk != null) ? dsdbk.getClass() : "NULL");

		// Until we do this, all the instruments are still *AVAIL*, but not LOADED.
		dsynth.open();

		Instrument[] loadedInstrms = dsynth.getLoadedInstruments();
		Instrument[] availInstrms = dsynth.getAvailableInstruments();
		getLogger().info("Instrument counts: loaded={}, avail={}", loadedInstrms.length, availInstrms.length);
		getLogger().info("Loaded Instruments: {}", loadedInstrms);
		getLogger().info("Avail Instruments: {}", availInstrms);
		StringBuffer all = new StringBuffer();
		List<Instrument> drumInstrs = new ArrayList<Instrument>();
		List<Instrument> keyInstrs = new ArrayList<Instrument>();
		for (Instrument inst : availInstrms) {
			Patch instPatch = inst.getPatch();
			getLogger().info("Instrument name=[{}] class=[{}] data-class=[{}] ", inst.getName(), inst.getClass(), inst.getDataClass());
			all.append("[" + inst.toString() + "], ");
			String instrDumpTxt = inst.toString();
			// Based on the toString() impl of com.sun.media.sound.DLSInstrument
			// http://grepcode.com/file/repository.grepcode.com/java/root/jdk/openjdk/6-b14/com/sun/media/sound/DLSInstrument.java#DLSInstrument.toString
			if (instrDumpTxt.toLowerCase().contains("drumkit")) {
				drumInstrs.add(inst);
			} else {
				keyInstrs.add(inst);
			}

		}

		getLogger().info("All avail instruments: {}", all.toString());

		VoiceStatus[] synthVoiceStatusArr = dsynth.getVoiceStatus();
		getLogger().info("VoiceStatus array size={}, firstEntry={}", synthVoiceStatusArr.length, synthVoiceStatusArr[0]);

		MidiChannel[] midChans = dsynth.getChannels();
		for (int cidx = 0; cidx < midChans.length; cidx++) {
			MidiChannel chan = midChans[cidx];
			getLogger().info("****************************\nChannel at position {} is {} ", cidx, midChans[cidx]);
			if (chan != null) {
				printChanFacts(chan);
			}
		}

		Receiver synthRcvr = dsynth.getReceiver();

		int onVelocity = 93, delayMsec = 75;
		Instrument[] dka = drumInstrs.toArray(new Instrument[0]);
		drumkitsTest(synthRcvr, dka, onVelocity, delayMsec);
		patchSwitchTest(synthRcvr, loadedInstrms, onVelocity, delayMsec);

		int drumInstIdx = 0, keyInstIdx = 0;
		int drumChanIdx = 9, keyChanIdx = 7;

		// We prefer the message sending form above, although the channels may also be treated as invokable.method().
		// The latter API does not (AFAWK) apply to general "device" channels, only to local synthesizers, so we
		// prefer to invest in wrapping the more general "message" approach above, which also maps well into our
		// ontology approach.
		//	channel.noteOn(nNoteNumber, nVelocity);
		//	channel.noteOff(nNoteNumber);

		Thread.sleep(2000);

		dsynth.close();

	}

	private void patchSwitchTest(Receiver synthRcvr, Instrument[] loadedInstrms, int onVel, int interSleepMsec) throws Throwable {
		ShortMessage noteOnMsg = new ShortMessage();
		ShortMessage progChangeMsg = new ShortMessage();
		ShortMessage notesOffMsg = new ShortMessage();
		for (int j = 0; j < 18; j++) {
			for (int i = 0; i < 16; i++) {
				int instIdx = (i + 16 * j) % (loadedInstrms.length);
				Instrument tgtInst = loadedInstrms[instIdx];

				Patch tgtPatch = tgtInst.getPatch();

				int noteIdx = 59 + i;
				int offVel = 0;
				int timeStampImmediate = -1;

				notesOffMsg.setMessage(ShortMessage.NOTE_OFF, i, noteIdx, offVel);
				synthRcvr.send(notesOffMsg, timeStampImmediate);
				getLogger().debug("Program change chanFromZ={} to {}", i, tgtInst);
				// Appears that sending bank this way is not univerally understood.
				// Another way (more official?) is to seperately select the bank as a CC-#0 value.
				progChangeMsg.setMessage(ShortMessage.PROGRAM_CHANGE, i, tgtPatch.getProgram(), tgtPatch.getBank());
				synthRcvr.send(progChangeMsg, timeStampImmediate);
				noteOnMsg.setMessage(ShortMessage.NOTE_ON, i, noteIdx, onVel);
				// Nice riff!  Note that when we play on channel "10" = 9, we get drum hits rather than "notes" 
				synthRcvr.send(noteOnMsg, timeStampImmediate);
				Thread.sleep(interSleepMsec);
			}
		}
	}

	private void drumkitsTest(Receiver synthRcvr, Instrument[] drumInstrums, int onVel, int interSleepMsec) throws Throwable {
		ShortMessage noteOnMsg = new ShortMessage();
		ShortMessage progChangeMsg = new ShortMessage();
		ShortMessage notesOffMsg = new ShortMessage();
		ShortMessage allNotesOffMsg = new ShortMessage();
		int drumChanIdx = 9;
		for (int j = 0; j < drumInstrums.length; j++) {
			Instrument tgtInst = drumInstrums[j];
			Patch tgtPatch = tgtInst.getPatch();
			getLogger().debug("Program change chanFromZ={} to {}", drumChanIdx, tgtInst);
			int timeStampImmediate = -1;

			progChangeMsg.setMessage(ShortMessage.PROGRAM_CHANGE, drumChanIdx, tgtPatch.getProgram(), tgtPatch.getBank());
			synthRcvr.send(progChangeMsg, timeStampImmediate);
			for (int noteIdx = 0; noteIdx < 128; noteIdx++) {

				int offVel = 0;

				notesOffMsg.setMessage(ShortMessage.NOTE_OFF, drumChanIdx, noteIdx, offVel);
				synthRcvr.send(notesOffMsg, timeStampImmediate);

				noteOnMsg.setMessage(ShortMessage.NOTE_ON, drumChanIdx, noteIdx, onVel);
				// Nice riff!  Note that when we play on channel "10" = 9, we get drum hits rather than "notes" 
				synthRcvr.send(noteOnMsg, timeStampImmediate);
				Thread.sleep(interSleepMsec);
			}
			int CC_allNotesOff = 123;
			int valIgnored = 0;
			allNotesOffMsg.setMessage(ShortMessage.CONTROL_CHANGE, drumChanIdx, CC_allNotesOff, valIgnored);
			synthRcvr.send(allNotesOffMsg, timeStampImmediate);
		}

	}

	private void callAllChannelMutators(MidiChannel chan) {
		int bendVal = 0, noteNum = 0, pressure = 0, program = 0, bank = 0, controllerNum = 0, ctrlValue = 0;

		chan.allNotesOff();
		chan.allSoundOff();
		chan.resetAllControllers();
		chan.controlChange(controllerNum, ctrlValue);
		chan.localControl(true);
		chan.setChannelPressure(pressure);
		chan.setPolyPressure(noteNum, pressure);
		chan.programChange(program); // In "currently selected" bank - where does that state live? 
		chan.programChange(bank, program);
		chan.setPitchBend(bendVal);
		boolean soloState = false, muteState = false, omniState = false, monoState = false, localControlState = false;
		chan.setSolo(soloState);
		chan.setMute(muteState);
		chan.setOmni(omniState);
		chan.setMono(monoState);
		chan.localControl(localControlState);

	}

	private void testMPMP() {
		// DemoMidiSeq.unitTestMPMP();
		int lengthMsec = 2000;
		DemoMidiSeq.shortTestPlay(DemoMidiSeq.DemoMonoMelody.GREENSLEEVES_MELODY, lengthMsec);
		DemoMidiSeq.shortTestPlay(DemoMidiSeq.DemoMonoMelody.DOUJAN, lengthMsec);
		DemoMidiSeq.shortTestPlay(DemoMidiSeq.DemoMonoMelody.BEETHOVEN_MOONLIGHT, lengthMsec);
		DemoMidiSeq.shortTestPlay(DemoMidiSeq.DemoMonoMelody.AULD_LANG_SYNE, lengthMsec);
		DemoMidiSeq.shortTestPlay(DemoMidiSeq.DemoMonoMelody.GOOD_KING_WENCESLAS, lengthMsec);
		DemoMidiSeq.shortTestPlay(DemoMidiSeq.DemoMonoMelody.GREEN_THREECOUNT, lengthMsec);
	}

	private void closeAllDevsAndExit() {
		/*  Iterator iterator = sm_openedMidiDeviceList.iterator(); while (iterator.hasNext())	{
		 MidiDevice	device = (MidiDevice) iterator.next();
		 device.close();
		 } if (DEBUG) { out("MidiPlayer.<...>.meta(): ...closed, now exiting"); }	 //System.exit(0);	 */
	}

	private void synthMapperCode(Sequencer seqr) throws Throwable {
		// This is just a bunch of nonsensical but compilable code to validate that we *could* call these methods.
		ArrayList<MidiDevice> sm_openedMidiDeviceList = new ArrayList<MidiDevice>();

		Transmitter midiTransmitter = seqr.getTransmitter();
		// Synth-receiver target
		Synthesizer synth = MidiSystem.getSynthesizer();
		synth.open();
		sm_openedMidiDeviceList.add(synth);
		Receiver synthReceiver = synth.getReceiver();
		Transmitter seqTransmitter = seqr.getTransmitter();
		seqTransmitter.setReceiver(synthReceiver);

		// OR: Default receiver target:
		Receiver midiReceiver = MidiSystem.getReceiver();
		midiTransmitter.setReceiver(midiReceiver);

		// OR:  specific dev version		
		MidiDevice.Info info = null; // new MidiDevice.Info(); //  MidiCommon.getMidiDeviceInfo(devName, true);
		MidiDevice midiDevice = MidiSystem.getMidiDevice(info);
		midiDevice.open();
		sm_openedMidiDeviceList.add(midiDevice);
		Receiver midiDevReceiver = midiDevice.getReceiver();
		midiTransmitter.setReceiver(midiDevReceiver);
		// Later
		midiDevReceiver.close();
		midiDevice.close();

		// Dumping sequencer output to text, or our intermediate
		//	Receiver	dumpReceiver = new DumpReceiver(System.out);
		//	Transmitter	dumpTransmitter = sm_sequencer.getTransmitter();
		//	dumpTransmitter.setReceiver(dumpReceiver);	

	}
}

/*
 void	noteOff(int noteNumber)
 Turns the specified note off.* 
 * void	noteOff(int noteNumber, int velocity)

 void	noteOn(int noteNumber, int velocity)
 Starts the specified note sounding.
 * 
 * 			nChannelNumber = Math.min(15, Math.max(0, nChannelNumber));
 nNoteNumberArgIndex = 1;
 // FALL THROUGH

 case 3:
 nNoteNumber = Integer.parseInt(args[nNoteNumberArgIndex]);
 nNoteNumber = Math.min(127, Math.max(0, nNoteNumber));
 nVelocity = Integer.parseInt(args[nNoteNumberArgIndex + 1]);
 nVelocity = Math.min(127, Math.max(0, nVelocity));
 nDuration = Integer.parseInt(args[nNoteNumberArgIndex + 2]);
 nDuration = Math.max(0, nDuration);
 * 
 * 	<formalpara><title>Bugs, limitations</title>
 <para>The precision of the duration depends on the precision
 of <function>Thread.sleep()</function>, which in turn depends on
 the precision of the system time and the latency of th
 thread scheduling of the Java VM. For many VMs, this
 means about 20 ms. When playing multiple notes, it is
 recommended to use a <classname>Sequence</classname> and the
 <classname>Sequencer</classname>, which is supposed to give better
 timing.</para>
 </formalpara>
 * 
 * 
 */
