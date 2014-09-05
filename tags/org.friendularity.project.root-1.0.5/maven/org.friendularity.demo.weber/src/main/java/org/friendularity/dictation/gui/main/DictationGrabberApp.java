/*
 * DictationGrabberApp.java
 */

package org.friendularity.dictation.gui.main;

import java.util.logging.Logger;
import org.jdesktop.application.Application;
import org.jdesktop.application.SingleFrameApplication;


/**
 * The main class of the application.
 */
public class DictationGrabberApp extends SingleFrameApplication {
    private static Logger	theLogger = Logger.getLogger(DictationGrabberApp.class.getName());

	DictationGrabberView	myDGV;
	//CogbotAvatar		myCSB;
    /**
     * At startup create and show the main frame of the application.
     */
    @Override protected void startup() {
		myDGV = new DictationGrabberView(this);
		show(myDGV);
    }

    /**
     * This method is to initialize the specified window by injecting resources.
     * Windows shown in our application come fully initialized from the GUI
     * builder, so this additional configuration is not needed.
     */
    @Override protected void configureWindow(java.awt.Window root) {
    }

    /**
     * A convenient static getter for the application instance.
     * @return the instance of DictationGrabberApp
     */
    public static DictationGrabberApp getApplication() {
        return Application.getInstance(DictationGrabberApp.class);
    }

    /**
     * Main method launching the application.
     */
    public static void main(String[] args) {
        launch(DictationGrabberApp.class, args);
    }
}
