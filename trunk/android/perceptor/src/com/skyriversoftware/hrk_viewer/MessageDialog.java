package com.skyriversoftware.hrk_viewer;

import android.app.AlertDialog;
import android.app.Dialog;
import android.app.DialogFragment;
import android.content.DialogInterface;
import android.os.Bundle;

public class MessageDialog extends DialogFragment {

    public MessageDialog(String title, String message) {
        Bundle args = new Bundle();
        // Why not just fields? Well, these args are way more Android-y! (Actually from the example, not sure if there's any advantage)
        args.putString("title", title);
        args.putString("message", message);
        setArguments(args);
    }

    @Override
    public Dialog onCreateDialog(Bundle savedInstanceState) {
        String title = getArguments().getString("title");
        String message = getArguments().getString("message");

        return new AlertDialog.Builder(getActivity())
                .setTitle(title)
                .setMessage(message)
                .setNeutralButton(R.string.alert_dialog_neutral,
                    new DialogInterface.OnClickListener() {
                        public void onClick(DialogInterface dialog, int whichButton) {
                        	dialog.dismiss();
                        }
                    }
                )
                .create();
    }
}
