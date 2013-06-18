/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package com.robosteps.org.friendularity.bundle.speechtiming;

import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;
import org.jflux.api.core.Listener;
import org.robokind.api.speech.SpeechEvent;
import org.robokind.api.speech.SpeechEventList;
import org.robokind.api.speech.SpeechJob;
import org.robokind.api.speech.messaging.RemoteSpeechServiceClient;
import org.robokind.api.speech.utils.DefaultSpeechJob;
import org.robokind.client.basic.Robokind;
import org.robokind.client.basic.UserSettings;

/**
 *
 * @author matt
 */
public class App {
    private static RemoteSpeechServiceClient mySpeaker;
    
    static String lastPhrase;
    public static void main( String[] args ){
        System.out.println("Enter the TTS ip (press enter to use 127.0.0.1): ");
        Scanner reader = new Scanner(System.in);
        String ip = reader.nextLine().trim();
        if(!ip.isEmpty()){
            UserSettings.setSensorAddress(ip);
        }
        mySpeaker = Robokind.connectSpeechService();
        mySpeaker.addSpeechEventListener(new SpeechEventListener());
        while(true){
            System.out.print("Enter a phrase: ");
            lastPhrase = reader.nextLine().trim();
            if(lastPhrase.equals(":q")){
                break;
            }
            SpeechJob job = mySpeaker.speak(lastPhrase);
            while(job.getStatus() <= DefaultSpeechJob.RUNNING){
                Robokind.sleep(5);
            }
        }
        
        Robokind.disconnect();
        System.exit(0);
    }
    
    static class SpeechEventListener implements Listener<SpeechEventList<SpeechEvent>> {
        SpeechInfo info;
        public void handleEvent(SpeechEventList<SpeechEvent> t) {
            if(t == null){
                return;
            }
            for(SpeechEvent e : t.getSpeechEvents()){
                handleSpeechEvent(e);
            }
        }
        
        
        private void handleSpeechEvent(SpeechEvent event) {
            if(SpeechEvent.SPEECH_START.equals(event.getEventType())){
                startSpeech();
            }else if(SpeechEvent.SPEECH_END.equals(event.getEventType())){
                endSpeech();
            }
            if(info == null){
                return;
            }
            if(!SpeechEvent.WORD_BOUNDARY.equals(event.getEventType())){
                return;
            }
            long bytePos = event.getTextPosition();
            long wordTime = (long)(((double)bytePos)*(1.0/16000.0)*1000.0);
            int wordStartIndex = event.getTextLength();
            info.wordsInfo.add(new SpeechWordInfo(wordStartIndex, wordTime));
            
//            StringBuilder sb = new StringBuilder();
//            sb.append("Text Position: ").append(event.getTextPosition()).append("\n");
//            sb.append("Text Length: ").append(event.getTextLength()).append("\n");
//            sb.append("Duration: ").append(event.getDuration()).append("\n");
//            sb.append("Current Data: ").append(event.getCurrentData()).append("\n");
//            System.out.println(sb.toString());
        }
        
        private void endSpeech(){
            for(int i=0; i<info.wordsInfo.size(); i++){
                SpeechWordInfo j = info.wordsInfo.get(i);
                int iJ = j.index;
                int iK = info.phrase.length();
                if(i+1 < info.wordsInfo.size()){
                    SpeechWordInfo k = info.wordsInfo.get(i+1);
                    iK =  (k.index > iJ) ? k.index : iJ+1;
                }
                String word = info.phrase.substring(iJ, iK);
                System.out.println("(" + j.startTimeMsec + ")\t" + word);
            }
        }
        
        private void startSpeech(){
            info = new SpeechInfo();
            info.phrase = lastPhrase;
            info.wordsInfo = new ArrayList<SpeechWordInfo>();
        }
    }
    
    static class SpeechWordInfo {
        int index;
        long startTimeMsec;

        public SpeechWordInfo(int index, long startTimeMsec) {
            this.index = index;
            this.startTimeMsec = startTimeMsec;
        }
    }
    
    static class SpeechInfo {
        String phrase;
        List<SpeechWordInfo> wordsInfo;
    }
}
