package com.skyriversoftware.hrk_viewer;

import java.util.ArrayList;
import java.util.Timer;

import android.app.Activity;
import android.app.DialogFragment;
import android.content.Context;
import android.os.Bundle;
import android.speech.RecognitionListener;
//import android.speech.RecognizerIntent;
import android.speech.SpeechRecognizer;
import android.util.Log;
//import android.widget.Toast;

public class SpeechProcessor implements RecognitionListener {
	
	static WebAppBox mainActivity;
	private Context mContext;
	
	private static long continuousRequestDelayTime = -1; // Holds time delay in ms between receiving text from a speech request and starting new request in "continuous" mode
	
	private static Timer runningTimer;
	
	private static DialogFragment speechAlert;
	
	
	
	SpeechProcessor(Activity app) {
		mainActivity = (WebAppBox)app;
		mContext = app;
		if (continuousRequestDelayTime < 0) {continuousRequestDelayTime = Long.valueOf(mainActivity.getResources().getInteger(R.integer.delay_between_continuous_speech_inputs));} 
	}
	
	public static void setContinuousRequestDelayTime(long delay) {
		continuousRequestDelayTime = delay;
	}
	
	public static Long getContinuousRequestDelayTime() {
		return continuousRequestDelayTime;
	}
	
	// WebAppBox sets this to the "Robot is listening" dialog when requesting speech
	public static void setSpeechAlertToClose(DialogFragment frag) {
		speechAlert = frag;
	}
	
	// A method that will close the "Robot is listening" dialog, if any is set
	private static void closeSpeechAlert() {
		if (speechAlert != null) {
			speechAlert.dismiss();
			speechAlert = null;
		}
	}
	
	@Override
    public void onResults(Bundle results) {
		closeSpeechAlert();
		ArrayList<String> matches = results.getStringArrayList(SpeechRecognizer.RESULTS_RECOGNITION);
        if (matches == null) {
            Log.e("SpeechProcessor.onResults", "No voice results");
        } else {
            Log.i("SpeechProcessor.onResults", "Printing matches: ");
            for (String match : matches) {
                Log.i("SpeechProcessor.onResults", match);
            }
            //Toast.makeText(mContext, "Google thinks you said \"" + matches.get(0) + "\"", Toast.LENGTH_LONG).show();
            SpeechUtil.postSpeech(matches.get(0));
            if (WebAppBox.getContinousSpeechMode()) {
            	Log.i("SpeechProcessor.onResults", "Scheduling another speech request since continuous mode is enabled...");
            	runningTimer = new Timer();
            	runningTimer.schedule(new RequestSpeechTimer(mainActivity), continuousRequestDelayTime);
            } else {
            	//SpeechUtil.setRequestingId(null); // Probably a good practice to set this back to null once we are done with this Lifter speech request
            	// Well this gets processed before the SpeechUtil.postSpeech thread is done, so blanks the id
            	// before we're ready for that to happen. Need to find a more robust way of passing/accounting for these
            	// requesting ids in upcoming refactoring
            }
        }
    }

    @Override
    public void onReadyForSpeech(Bundle params) {
        Log.d("SpeechProcessor.onReadyForSpeech", "Ready for speech");
    }

    @Override
    public void onError(int error) {
    	closeSpeechAlert();
        Log.e("SpeechProcessor.onError", "Error listening for speech: " + error);
        if (error != SpeechRecognizer.ERROR_NO_MATCH) {
        	//Toast.makeText(mContext, "Oops, Google is reporting a problem with speech recognition! Try again in a minute.", Toast.LENGTH_LONG).show();
        	DialogFragment newFragment = new MessageDialog(mainActivity.getResources().getString(R.string.speech_problem_title),
        			mainActivity.getResources().getString(R.string.general_speech_req_problem));
            newFragment.show(mainActivity.getFragmentManager(), "general_speech_req_problem_dialog");
        	mainActivity.resetSpeechRecognizer(); // This may help
        } else {
        	//Toast.makeText(mContext, "Google didn't understand you. Try again.", Toast.LENGTH_LONG).show();
        	DialogFragment newFragment = new MessageDialog(mainActivity.getResources().getString(R.string.speech_problem_title),
        			mainActivity.getResources().getString(R.string.no_match_speech_req_problem));
            newFragment.show(mainActivity.getFragmentManager(), "no_match_speech_req_problem_dialog");
        }
        if (WebAppBox.getContinousSpeechMode()) {
        	runningTimer = new Timer();
        	runningTimer.schedule(new RequestSpeechTimer(mainActivity), Long.valueOf(mainActivity.getResources().getInteger(R.integer.delay_after_continuous_speech_error)));
        }
    }

    @Override
    public void onBeginningOfSpeech() {
        Log.d("SpeechProcessor.onBeginningOfSpeech", "Speech starting");
    }
    
    @Override
    public void onBufferReceived(byte[] buffer) {
        //Log.d("SpeechProcessor.onBufferReceived", "Buffer Received");
    }
    
    @Override
    public void onEndOfSpeech() {
        Log.d("SpeechProcessor.onEndOfSpeech", "End of Speech Notification Received");
    }
    
    @Override
    public void onEvent(int eventType, Bundle params) {
        Log.e("SpeechProcessor.onEvent", "Event signaled: Type " + eventType);
    }
    
    @Override
    public void onPartialResults(Bundle results) {
        ArrayList<String> matches = results.getStringArrayList(SpeechRecognizer.RESULTS_RECOGNITION);
        if (matches == null) {
            Log.e("SpeechProcessor.onPartialResults", "No voice partial results");
        } else {
            Log.i("SpeechProcessor.onPartialResults", "Printing matches: ");
            for (String match : matches) {
                Log.i("SpeechProcessor.onPartialResults", match);
            }
            //Toast.makeText(mContext, "Google thinks you said \"" + matches.get(0) + "\"", Toast.LENGTH_LONG).show();
            //SpeechUtil.postSpeech(matches.get(0));
        }
    }
    
    @Override
    public void onRmsChanged(float rmsdB) {
        //Log.e("SpeechProcessor.onRmsChanged", "Sound Level RMS changed to " + rmsdB);
    }
    
    static void stopTimer() {
     	try {
     		runningTimer.cancel();
    	} catch (Exception e) {
    		Log.i("SpeechProcessor.stopTimer", "Error stopping continuous speech timer, maybe because it wasn't running: " + e);
    	}
    }
}
