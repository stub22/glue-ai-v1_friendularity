/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.friendularity.speech;

import org.friendularity.dictation.gui.main.DictationImpl;
import org.friendularity.dictation.gui.main.VolumeMonitorPanel;
import org.friendularity.dictation.jmx.JMXInterface;
import javax.sound.sampled.AudioFormat;
import javax.sound.sampled.AudioSystem;
import javax.sound.sampled.DataLine;
import javax.sound.sampled.LineUnavailableException;
import javax.sound.sampled.TargetDataLine;
import java.util.Properties;
/**
 *
 * @author humankind
 */
public class SoundDetector extends java.util.Observable {
    public enum SoundState{
        SOUND,
        LONG_SOUND,
        SHORT_SILENCE,
        LONG_SILENCE
    }
    private SoundState myState;
    private DictationImpl   myDictationImpl;
    private boolean	    mySendOnSilence = false;
    private boolean     mySendSoundCues = false;
    private TargetDataLine  myDataLine;
    private Long            mySendWaitTime = 50L;

    private CaptureThread myCaptureThread;

    private Properties myProperties;

    private VolumeMonitorPanel myVolumeMonitorPanel = null;

    public static void main(String[] args) {
        new SoundDetector(null, org.friendularity.dictation.main.Utils.loadProperties("C:/_hanson/_deploy/distro_18d/conf/_dictation_grabber/dictation.properties"));
    }

    public SoundDetector(DictationImpl di, Properties props){
        this(di,null,props);
        myState = SoundState.SHORT_SILENCE;
    }

    public SoundDetector(DictationImpl di, VolumeMonitorPanel vmp, Properties props){
        myDictationImpl = di;
        myVolumeMonitorPanel = vmp;
        myProperties=props;
    }

    public Long getSendWaitTime() {
        return mySendWaitTime;
    }

    public void setSendWaitTime(Long mySendWaitTime) {
        this.mySendWaitTime = mySendWaitTime;
    }

    public void initialize(){
            AudioFormat audioFormat = getAudioFormat();
            DataLine.Info dataLineInfo =  new DataLine.Info(TargetDataLine.class, audioFormat);
            try{
                    myDataLine = (TargetDataLine)AudioSystem.getLine(dataLineInfo);
                    myDataLine.open(audioFormat);
                    myDataLine.start();
                    myCaptureThread = new CaptureThread(this);
                    myCaptureThread.start();
            }catch(LineUnavailableException ex){}
    }
	
    private AudioFormat getAudioFormat(){
            float sampleRate;
            int sampleSizeInBits;
            int channels;
            boolean signed;
            boolean bigEndian;
            //sampleRate = 44100.0F;    //8000,11025,16000,22050,44100
            //sampleSizeInBits = 16;	//8,16
            //channels = 2;				//1,2

            //sampleRate = 4096.0F;	    //8000,11025,16000,22050,44100
            //sampleSizeInBits = 8;		//8,16
            //channels = 1;				//1,2
            
            sampleRate      =Float.parseFloat(myProperties.getProperty("TargetDataLineSampleRate"));
            sampleSizeInBits=Integer.parseInt(myProperties.getProperty("TargetDataLineSampleSizeInBits"));
            channels        =Integer.parseInt(myProperties.getProperty("TargetDataLineChannels"));
            signed          =true;
            bigEndian       =true;
            return new AudioFormat(sampleRate, sampleSizeInBits, channels, signed, bigEndian);
    }

    public boolean getSendOnSilence(){
        return mySendOnSilence;
    }

    public void setSendSoundCues(boolean bool){
        mySendSoundCues = bool;
    }

    public boolean getSendSoundCues(){
        return mySendSoundCues;
    }

    public void setSendOnSilence(boolean bool){
        mySendOnSilence = bool;
    }

    public synchronized void shortSilenceDetected() {
        myState = SoundState.SHORT_SILENCE;
        print("Short Silence\n");
        if(mySendSoundCues){
            JMXInterface.sendUtterance("ShortSilence");
        }
    }

    private static boolean sendLocked = false;
    public synchronized void longSilenceDetected()  {
        myState = SoundState.LONG_SILENCE;
        print("Long Silence\n");
        if(sendLocked){
            return;
        }
        try{
            sendLocked = true;
            if(mySendSoundCues){
                JMXInterface.sendUtterance("LongSilence");
            }
            if(!mySendOnSilence){
               return;
            }
            boolean update = false;
            while(myDictationImpl.updateInput()){
                update = true;
                try{
                    Thread.sleep(mySendWaitTime);
                }catch(Throwable t){}
            }
            if(update){
                myDictationImpl.SendMatches();
            }
        }finally{
            sendLocked = false;
        }
    }

    public synchronized void soundStartDetected() {
        myState = SoundState.SOUND;
        print("\nSound Detected\n");
        if(mySendSoundCues){
            JMXInterface.sendUtterance("Sound");
        }
    }

    public synchronized void veryLongSoundDetected(){
        myState = SoundState.LONG_SOUND;
        print("Long Sound Detected\n");
        if(mySendSoundCues){
            JMXInterface.sendUtterance("LongSound");
        }
    }
    
    public TargetDataLine getDataLine()   {
        return myDataLine;
    }

    public CaptureThread getCaptureThread() {
        return myCaptureThread;
    }

    public void setSoundConfigs(double silenceThreshold, long minSoundLength, long veryLongSoundLength, long shortSilenceLength, long longSilenceLength, int bufferSize) {
        myCaptureThread.setSoundConfigs(silenceThreshold,  minSoundLength,  veryLongSoundLength, shortSilenceLength,  longSilenceLength, bufferSize);
    }

    private void print(String s) {
       //System.out.println(s);
       if(myVolumeMonitorPanel!=null) {
           myVolumeMonitorPanel.appendStatusInfo(s);
       }
    }

    public SoundState getState(){
        return myState;
    }

}
