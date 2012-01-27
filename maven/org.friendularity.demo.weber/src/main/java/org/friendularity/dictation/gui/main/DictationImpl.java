package org.friendularity.dictation.gui.main;

import org.friendularity.dictation.main.Utils;
import org.friendularity.dictation.main.VocabularyManager;
import org.friendularity.dictation.jmx.JMXInterface;
import org.friendularity.dictation.jmx.JMXWrapper;
import org.friendularity.speech.SoundDetector;
import org.friendularity.speech.CaptureThread;
import java.util.Properties;

/**
 *
 * @author Matt Stevenson
 */
public class DictationImpl {
	public static String thePropsFilePath = "C:/_hanson/_deploy/distro_20a/conf/_dictation_grabber/dictation.properties";
    Double DefaultServerThreshold = 0.0;
    VocabularyManager myVocabManager;
    DictationPanel myPanel;
    private SoundDetector mySoundDetector;
    private Properties myProperties;

    public DictationImpl(DictationPanel dp){
        myPanel = dp;
        loadINI();
        mySoundDetector = new SoundDetector(this, myPanel.getVolumeMonitorPanel(), myProperties);
      
	//	mySoundDetector.initialize();
        loadSoundLengths();

        CaptureThread capThread=mySoundDetector.getCaptureThread();
        String minSoundLengthText=""+capThread.getMinSoundLength();
        String veryLongSoundLengthText=""+capThread.getVeryLongSoundLength();
        String shortSilenceLengthText=""+capThread.getShortSilenceLength();
        String longSilenceLengthText=""+capThread.getLongSilenceLength();
        String sendWait=""+(long)mySoundDetector.getSendWaitTime();
        String volumeThresholdText=""+capThread.getVolumeThreshold();
        String bufferSizeText=""+capThread.getBufferSize();

        myPanel.setSoundConfigUIValues(minSoundLengthText, veryLongSoundLengthText, 
                shortSilenceLengthText, longSilenceLengthText, volumeThresholdText,
                bufferSizeText, sendWait, mySoundDetector.getSendOnSilence() );
    }

    public VocabularyManager getVocabManager(){
        return myVocabManager;
    }

    private void loadINI() {
        try{
            myProperties = Utils.loadProperties(thePropsFilePath);
            String xmlPath = myProperties.getProperty("VocabularyDirectory");
            String logDirectory = myProperties.getProperty("LogDirectory");
            String answersDirectory = myProperties.getProperty("AnswersDirectory");
            myVocabManager = VocabularyManager.loadManagerFromFile(xmlPath,logDirectory);
            myVocabManager.addVocabularies(VocabularyManager.loadVocabulariesFromFile(answersDirectory));
            myVocabManager.setTextArea(myPanel.txtMatches);
            JMXInterface.setProperties(myProperties);
        }catch(Throwable t){t.printStackTrace();}
    }

    private void loadSoundLengths() {
        try{
            int minSoundLength = Integer.parseInt(myProperties.getProperty("MinSoundLength"));
            int veryLongSoundLength = Integer.parseInt(myProperties.getProperty("VeryLongSoundLength"));
            int shortSilenceLength = Integer.parseInt(myProperties.getProperty("ShortSilenceLength"));
            int longSilenceLength = Integer.parseInt(myProperties.getProperty("LongSilenceLength"));
            int sendWait = Integer.parseInt(myProperties.getProperty("SendWait"));
            double volumeThreshold = Double.parseDouble(myProperties.getProperty("VolumeThreshold"));
            int bufferSize = Integer.parseInt(myProperties.getProperty("BufferSize"));
            setSoundConfigs(volumeThreshold, minSoundLength, veryLongSoundLength, shortSilenceLength, longSilenceLength, bufferSize, sendWait);
        }catch(Throwable t){t.printStackTrace();}
    }

    public void SendMatches(){
        myVocabManager.SendMatches(new JMXWrapper());
    }

    public boolean updateInput() {
        String input = myPanel.txtConcatInput.getText();
        myVocabManager.appendInput(input);
        myPanel.txtConcatInput.setText("");
        return !input.isEmpty();
    }
	public void receiveNetworkText(String text) {
		myVocabManager.appendInput(text);
		SendMatches();
	}
    public SoundDetector getSoundDetector() {
        return mySoundDetector;
    }
    public void setSoundConfigs(double silenceThreshold, long minSoundLength,
                                long veryLongSoundLength, long shortSilenceLength,
                                long longSilenceLength, int bufferSize,
                                long sendWait) {
        mySoundDetector.setSoundConfigs(silenceThreshold, minSoundLength, veryLongSoundLength, shortSilenceLength,  longSilenceLength, bufferSize);
        mySoundDetector.setSendWaitTime(sendWait);
    }
}