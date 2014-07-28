package com.skyriversoftware.hrk_viewer;


import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.os.Bundle;
import android.app.DialogFragment;

public class SpeechInputAlertFragment extends DialogFragment{
    
	/*
    @Override
    public Dialog onCreateDialog(Bundle savedInstanceState) {
    	Dialog theDialog = new AlertDialog.Builder(getActivity())
			   		   
			   	//.setView(View.inflate(getActivity(), R.layout.speech_input_alert_layout, (ViewGroup)getActivity().findViewById(R.id.webParentView)))
    			.setView(View.inflate(getActivity(), R.layout.speech_input_alert_layout, null))
		   	   
                .create();
        //theDialog.getWindow().setLayout(600, 400); //Controlling width and height.
        return theDialog;
                
    }
    */
	@Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setStyle(DialogFragment.STYLE_NO_TITLE, android.R.style.Theme_Holo_Light);
	}
	
    
    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
            Bundle savedInstanceState) {
    	//getDialog().setTitle("Speech Input:");
    	getDialog().getWindow().setLayout(400,150); // This is also set in layout, but doesn't seem to work there.
        View v = inflater.inflate(R.layout.speech_input_alert_layout, container, false);
        return v;
    }
      
}