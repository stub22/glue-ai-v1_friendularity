/*
 *  Copyright 2012 by Sky River Software (skyriversoftware.com).
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

package com.skyriversoftware.hrk_viewer;

//import java.util.ArrayList;
import java.util.List;

import android.app.Activity;
import android.app.AlertDialog;
import android.app.DialogFragment;
import android.app.FragmentTransaction;
import android.content.DialogInterface;
//import android.content.Context;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.content.pm.ResolveInfo;
import android.content.res.Resources;
import android.os.Bundle;
import android.speech.RecognitionListener;
import android.speech.RecognizerIntent;
import android.speech.SpeechRecognizer;
import android.speech.tts.TextToSpeech;
import android.speech.tts.TextToSpeech.OnInitListener;
import android.util.Log;
import android.view.Menu;
import android.view.MenuItem;
import android.view.MotionEvent;
import android.view.View;
import android.webkit.WebSettings;
import android.webkit.WebView;
import android.webkit.WebViewClient;
import android.widget.EditText;
//import android.widget.Toast;
import android.widget.TextView.BufferType;

/**
 *  * @author Ryan Biggs
 */
public class WebAppBox extends Activity implements JavaScriptInterface.JsListener, OnInitListener {

	WebView mainView;
	Resources res;
	
	private static TextToSpeech mTts;
	private static SpeechRecognizer mRecognizer;
	private static RecognitionListener mSpeechListener;
	
	private int myProcessId;  
	
	private static final int VOICE_RECOGNITION_REQUEST_CODE = 1234;
	private static final int TTS_DATA_CHECK_CODE = 1235;
	final static int OPTIONS_SET_MAIN_URL_ID = 1001;
	final static int OPTIONS_SET_PUSHY_URL_ID = 1002;
	final static int OPTIONS_SET_SPEECH_URL_ID = 1003;
	final static int OPTIONS_REFRESH_ID = 1004;
	final static int OPTIONS_EXIT_ID = 1005;
	final static int OPTIONS_SHOW_VERSION_ID = 1006;
	final static int OPTIONS_EXIT_CONTINUOUS_SPEECH_ID = 1007;
	final static int OPTIONS_GET_SPEECH_DELAY_TIME_ID = 1008;
	final static int ACTION_SET_SPEECH_DELAY_TIME = 2001;
	final static int ACTION_SET_SPEECH_PATH = 2002;
	
	private boolean onMainPage=false; //Set if WebAppBox is displaying the "main" webapp
	private static boolean continuousSpeechMode = false; // True when we are collecting speech in "continuous" mode 
	private String url; // URL of currently displayed page
	
	/**
     * The following method is run by Android upon creation of activity
     * 
     * @param savedInstanceState	Bundle of View state information used by Android upon restart
     */
	public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        
        // Get our process ID so we can kill our process if we want - oh the horrors!
        myProcessId = android.os.Process.myPid();
        
        /* This section prepares to launch the web app in an Android WebView
         * View class using the layout defined in webappbox_main.xml
         */
        setContentView(R.layout.webappbox_main);
        mainView = (WebView) findViewById(R.id.webview);
        WebSettings webSettings = mainView.getSettings();
        webSettings.setJavaScriptEnabled(true);
        mainView.addJavascriptInterface(new JavaScriptInterface(this), "Android");
        
        // Make things so that redirects still load in the WebView
        mainView.setWebViewClient(new WebViewClient());
        
        // disable scroll on touch -- even if viewport is set, user can still drag-scroll
        mainView.setOnTouchListener(new View.OnTouchListener() {
          public boolean onTouch(View v, MotionEvent event) {
            return (event.getAction() == MotionEvent.ACTION_MOVE);
          }
        });
        
        res = getResources();
        
        // Probably can do away with all this soon?
        url = getIntent().getStringExtra("appUri");
        if (url == null) { // If this is being run initially from home screen,
        	url = UrlRegistry.getMainUrl(); /// ... Load MainUrl
        	onMainPage = true; // Not really needed due to assignment in onResume
        	
    	// Get a fancy TextToSpeech engine!
		getTextToSpeech();	
        }
	}
	
	/* Added to maybe put getTextToSpeech here, but looks like may better live in onCreate
	public void onStart() {
		super.onStart();
		// Get a fancy TextToSpeech engine!
		getTextToSpeech();
	}
	*/
	
	/**
     * The following method is run by Android when Proctor activity moves to foreground
     */
	public void onResume() {
		super.onResume();
		
		// Set onMainPage to determine options menu visibility
		onMainPage = url.equals(UrlRegistry.getMainUrl()); // May not be necessary 
		
		/* Get desired URI from Intent and load into the WebView*/
		mainView.loadUrl(url);	
		
		/* Not so much, for HRK Viewer
		// "Dim" system bar
        mainView.setSystemUiVisibility(View.STATUS_BAR_HIDDEN);
        */
		
	}
	
	// Shutdown TextToSpeech and SpeechRecognizer when leaving activity
	public void onDestroy() {
		Log.d("WebAppBox.onDestroy", "In onDestroy...");
		super.onDestroy();
		if (mTts != null) {
			mTts.stop();
			mTts.shutdown();
		}
		if (mRecognizer != null) {
			mRecognizer.destroy();
		}
	}
	
	@Override
	public boolean onCreateOptionsMenu(Menu optionsMenu) {
		if (onMainPage) {
			optionsMenu.add(Menu.NONE, OPTIONS_REFRESH_ID, Menu.NONE, "Refresh current page");
			optionsMenu.add(Menu.NONE, OPTIONS_SET_MAIN_URL_ID, Menu.NONE, "Set web app URL");
			// optionsMenu.add(Menu.NONE, OPTIONS_SET_PUSHY_URL_ID, Menu.NONE, "Set \"Pushy\" webapp URL"); // Currently not needed for viewer
			optionsMenu.add(Menu.NONE, OPTIONS_SET_SPEECH_URL_ID, Menu.NONE, "Set path for JSON speech out PUT");
			optionsMenu.add(Menu.NONE, OPTIONS_GET_SPEECH_DELAY_TIME_ID, Menu.NONE, "Set time delay before next \"continuous\" speech request");
			optionsMenu.add(Menu.NONE, OPTIONS_SHOW_VERSION_ID, Menu.NONE, "Show Version info");
			optionsMenu.add(Menu.NONE, OPTIONS_EXIT_CONTINUOUS_SPEECH_ID, Menu.NONE, "Force Stop Continuous Speech Input");
	    	optionsMenu.add(Menu.NONE, OPTIONS_EXIT_ID, Menu.NONE, "Stop and Close App");
	    }
	    return true;
	}
	

	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
	    // Handle item selection
		int id = item.getItemId();
	    switch (id) {
		    case OPTIONS_SET_MAIN_URL_ID:
		        getUrlFromUser(UrlRegistry.getMainUrl(), id);
		        return true;
		    case OPTIONS_SET_PUSHY_URL_ID:
		        getUrlFromUser(UrlRegistry.getPushyUrl(), id);
		        return true;
		    case OPTIONS_SET_SPEECH_URL_ID:
		        getUserInput("Enter relative path for JSON speech out PUT", ACTION_SET_SPEECH_PATH, UrlRegistry.getSpeechPath());
		        return true;    
		    case OPTIONS_REFRESH_ID:
		    	Log.i("WebAppBox.onOptionsItemSelected", "Reloading page by menu request");
		        mainView.reload();
		        return true; 
		    case OPTIONS_EXIT_ID:
		        //this.finish();
		    	reallyQuit();
		        return true;
		    case OPTIONS_SHOW_VERSION_ID:
		        showVersionDialog();
		        return true;
		    case OPTIONS_EXIT_CONTINUOUS_SPEECH_ID:
		    	stopContinuousSpeechInput();
		        return true;
		    case OPTIONS_GET_SPEECH_DELAY_TIME_ID:
		    	Long currentValue = SpeechProcessor.getContinuousRequestDelayTime();
		    	if (currentValue == -1) {currentValue = Long.valueOf(res.getInteger(R.integer.delay_between_continuous_speech_inputs));} // It hasn't been set yet, use the default
		    	getUserInput("Enter new delay between recognition of speech and next request (ms)", ACTION_SET_SPEECH_DELAY_TIME, currentValue.toString());
		        return true;
	    default:
	        return super.onOptionsItemSelected(item);
	    }
	}
	
	private void reallyQuit() {
		onDestroy();
		//this.finish(); 
		android.os.Process.sendSignal(myProcessId, android.os.Process.SIGNAL_KILL);
	}

	
	private void getUrlFromUser(String currentURL, final int id) {
		final EditText input = new EditText(this);
		input.setText(currentURL, BufferType.EDITABLE);
		new AlertDialog.Builder(this)
	    .setTitle("Enter URL")
	    .setView(input)
	    .setPositiveButton("Ok", new DialogInterface.OnClickListener() {
	        public void onClick(DialogInterface dialog, int whichButton) {
	        	setUserUrl(input.getText().toString(), id);
	        }
	    }).setNegativeButton("Cancel", new DialogInterface.OnClickListener() {
	        public void onClick(DialogInterface dialog, int whichButton) {
	        }
	    }).show();
	}
	
	private void getUserInput(String prompt, final int action, String currentValue) {
		
		final EditText input = new EditText(this);
		input.setText(currentValue, BufferType.EDITABLE);
		new AlertDialog.Builder(this)
	    .setTitle(prompt)
	    .setView(input)
	    .setPositiveButton("Ok", new DialogInterface.OnClickListener() {
	        public void onClick(DialogInterface dialog, int whichButton) {
	        	actOnUserInput(action, input.getText().toString());
	        }
	    }).setNegativeButton("Cancel", new DialogInterface.OnClickListener() {
	        public void onClick(DialogInterface dialog, int whichButton) {
	        }
	    }).show();
	}
	
	private void actOnUserInput(int action, String results) {
		switch (action) {
			case ACTION_SET_SPEECH_DELAY_TIME:
				try {
					Long value = Long.valueOf(results);
					if (value >= 0) {
						SpeechProcessor.setContinuousRequestDelayTime(value);
					} else {
						//Toast.makeText(this, "Your input appears to be negative, ignoring", Toast.LENGTH_LONG).show();
						DialogFragment newFragment = new MessageDialog(res.getString(R.string.invalid_input),
				    			res.getString(R.string.negative_number));
				        newFragment.show(getFragmentManager(), "negative_number_dialog");
					}
				} catch (Exception e) {
					//Toast.makeText(this, "Your input was not recognized as a number", Toast.LENGTH_LONG).show();
					DialogFragment newFragment = new MessageDialog(res.getString(R.string.invalid_input),
			    			res.getString(R.string.not_a_number));
			        newFragment.show(getFragmentManager(), "not_a_number_dialog");
				}
				return;
			case ACTION_SET_SPEECH_PATH:
				UrlRegistry.setSpeechPath(results);
		}
	}
	
	void setUserUrl(String url, int id) {
		switch (id) {
			case OPTIONS_SET_MAIN_URL_ID:
		        UrlRegistry.setMainUrl(url);
		        mainView.loadUrl(url);
		        this.url = url;
		        return;
		    case OPTIONS_SET_PUSHY_URL_ID:
		        UrlRegistry.setPushyUrl(url);
		        return; 
		}
	}

	/**
     * SureLock does not defeat the back button, but that's easy, so we do it here
     * by overriding this method with an empty one.
     */
	/* Not so desirable for HRK Viewer
    @Override
    public void onBackPressed() {
       return;
    }
    */
    
    public void setUrl(String url) {
    	this.url = url;
    	Log.d("WebAppBox.setUrl", "Loading URL: " + url);
    	mainView.loadUrl(url);
    }
    
    @Override
    public void onVoiceInputRequested() {
    	// Check to see if a recognition activity is present
        PackageManager pm = getPackageManager();
        List<ResolveInfo> activities = pm.queryIntentActivities(
                new Intent(RecognizerIntent.ACTION_RECOGNIZE_SPEECH), 0);
        if (activities.size() != 0) {
        	startVoiceRecognitionActivity();
        } else {
        	//Toast.makeText(this, "Voice Input Requested, but recognizer app not present", Toast.LENGTH_LONG).show();
    	DialogFragment newFragment = new MessageDialog(res.getString(R.string.speech_problem_title),
    			res.getString(R.string.no_recognizer_speech_req_problem));
        newFragment.show(getFragmentManager(), "no_recognizer_speech_req_problem_dialog");
        }
    }
    
    @Override
    public void onVoiceOutputRequested(String text) {
    	Log.i("WebAppBox.onVoiceOutputRequested", "It's been requested that we say " + text);
    	if (mTts != null) {
    		mTts.speak(text, TextToSpeech.QUEUE_ADD, null);
    	} else {
    		Log.e("WebAppBox.onVoiceOutputRequested", "Speech output requested, but no TextToSpeech instance was successfully initialized");
    	}
    }
    
    @Override
    public void startContinuousSpeechInput() {
    	continuousSpeechMode = true;
    	onVoiceInputRequested();
    }
    
    @Override
    public void stopContinuousSpeechInput() {
    	Log.d("WebAppBox.stopContinuousSpeechInput", "Requesting stop of continuous speech input");
    	continuousSpeechMode = false;
    	SpeechProcessor.stopTimer(); // Stop any pending speech request timer
    }
    
    public static boolean getContinousSpeechMode() {
    	Log.d("WebAppBox.getContinuousSpeechMode", "is saying that continuous mode is " + continuousSpeechMode);
    	return continuousSpeechMode;
    }
    
    private void startVoiceRecognitionActivity() {
        Intent intent = new Intent(RecognizerIntent.ACTION_RECOGNIZE_SPEECH);

        // Specify the calling package to identify your application
        intent.putExtra(RecognizerIntent.EXTRA_CALLING_PACKAGE, getClass().getPackage().getName());

        // Display an hint to the user about what he should say.
        intent.putExtra(RecognizerIntent.EXTRA_PROMPT, "Speak to the robot!");

        // Given an hint to the recognizer about what the user is going to say
        intent.putExtra(RecognizerIntent.EXTRA_LANGUAGE_MODEL,
                RecognizerIntent.LANGUAGE_MODEL_FREE_FORM);

        // Specify how many results you want to receive. The results will be sorted
        // where the first result is the one with higher confidence.
        intent.putExtra(RecognizerIntent.EXTRA_MAX_RESULTS, 5);
        
        // This stuff maybe used to work [historically, I hear] but doesn't appear to now
        //intent.putExtra(RecognizerIntent.EXTRA_SPEECH_INPUT_COMPLETE_SILENCE_LENGTH_MILLIS, 10000);
        //intent.putExtra(RecognizerIntent.EXTRA_SPEECH_INPUT_POSSIBLY_COMPLETE_SILENCE_LENGTH_MILLIS, 10000);
        //intent.putExtra(RecognizerIntent.EXTRA_PARTIAL_RESULTS, true);

        
        // Get SpeechRecognizer if we don't have one already
        if (mRecognizer == null) {
        	mRecognizer = SpeechRecognizer.createSpeechRecognizer(this.getApplicationContext());
        }
        
        // ... and the same with RecognitionListener
        if (mSpeechListener == null) {
        	mSpeechListener = new SpeechProcessor(this);
        	mRecognizer.setRecognitionListener(mSpeechListener);
        }
        
        //Toast.makeText(this, "The robot is now listening!", Toast.LENGTH_LONG).show();
        // Make a dialog alert message to tell user that the robot is listening!
        DialogFragment alertFragment = new SpeechInputAlertFragment();
        FragmentTransaction ft = getFragmentManager().beginTransaction();
        alertFragment.show(ft, "speechReqActive");
        SpeechProcessor.setSpeechAlertToClose(alertFragment); // By setting this in SpeechProcessor, it will dismiss the dialog when the speech req is done
        mRecognizer.startListening(intent);
        
        //startActivityForResult(intent, VOICE_RECOGNITION_REQUEST_CODE); // Old way using speech activity
    }
    
    /**
     * Handle results from the speech recognition activity.
     * 
     */

    @Override
    protected void onActivityResult(int requestCode, int resultCode, Intent data) {
    	Log.d("WebAppBox.onActivityResult", "In onActivityResult");
    	/* No longer used with current SpeechProcessor scheme, should be going away soon.
        if (requestCode == VOICE_RECOGNITION_REQUEST_CODE && resultCode == RESULT_OK) {
            // Fill the list view with the strings the recognizer thought it could have heard
            ArrayList<String> matches = data.getStringArrayListExtra(
                    RecognizerIntent.EXTRA_RESULTS);
            Toast.makeText(this, "Google thinks you said \"" + matches.get(0) + "\"", Toast.LENGTH_LONG).show();
            SpeechUtil.postSpeech(matches.get(0));
            if (continuousSpeechMode) onVoiceInputRequested(); // If in continuous speech mode, collect speech again!
        }
        */
        if (requestCode == TTS_DATA_CHECK_CODE) {
            if (resultCode == TextToSpeech.Engine.CHECK_VOICE_DATA_PASS) {
                // success, create the TTS instance
                mTts = new TextToSpeech(this, this);
            } else {
                // missing data, install it
                Intent installIntent = new Intent();
                installIntent.setAction(
                    TextToSpeech.Engine.ACTION_INSTALL_TTS_DATA);
                startActivity(installIntent);
            }
        }
        super.onActivityResult(requestCode, resultCode, data);
    }

    
    void getTextToSpeech() {
    	Intent checkIntent = new Intent();
    	checkIntent.setAction(TextToSpeech.Engine.ACTION_CHECK_TTS_DATA);
    	startActivityForResult(checkIntent, TTS_DATA_CHECK_CODE);
    }
    
    void resetSpeechRecognizer() {
    	mRecognizer.destroy();
    	mRecognizer = SpeechRecognizer.createSpeechRecognizer(this.getApplicationContext());
        if (mSpeechListener == null) {
        	mSpeechListener = new SpeechProcessor(this);
        }
        mRecognizer.setRecognitionListener(mSpeechListener);
    }
    
    public void onInit(int status) {
    	if (status == TextToSpeech.SUCCESS) {
    		mTts.setLanguage(java.util.Locale.US);
    		mTts.setPitch(0.9f);
    	}
    }
    
    private void showVersionDialog() {
    	AlertDialog.Builder alertDialogBuilder = new AlertDialog.Builder(
				this);
 
			// set title
			alertDialogBuilder.setTitle("HRK Android Viewer");
 
			// set dialog message
			alertDialogBuilder
				.setMessage(res.getString(R.string.app_version))
				.setNeutralButton("OK",new DialogInterface.OnClickListener() {
					public void onClick(DialogInterface dialog,int id) {
						dialog.cancel();
					}
				  });

				// create alert dialog
				AlertDialog alertDialog = alertDialogBuilder.create();
 
				// show it
				alertDialog.show();
		}
    
}
 
