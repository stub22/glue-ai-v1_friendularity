package org.friendularity.jvision.gui;

import org.friendularity.jvision.gui.FileLocations;
import java.awt.color.ColorSpace;
import java.awt.image.BufferedImage;
import java.awt.image.WritableRaster;
import java.util.ArrayList;
import java.util.Formatter;
import org.friendularity.jvision.engine.JVisionEngine;
import org.friendularity.jvision.engine.Quitter;

import org.opencv.core.Core;
import org.opencv.core.CvType;
import org.opencv.core.Mat;
import org.opencv.highgui.*;
import org.opencv.imgproc.Imgproc;
import org.opencv.video.Video;
import org.opencv.utils.Converters;
import org.friendularity.jvision.gui.DemoFrame;
import org.friendularity.jvision.filters.FilterSequence;

public class JVisionLauncher implements Quitter {

	private DemoFrame		myDemoFrame;

	private	Thread			myCamProcThread;
	private Boolean			myQuittingFlag  = Boolean.FALSE;
	private JVisionEngine	myEngine;

	public static void main(String[] args) {
		// Can use this to run-file without bundling, if your IDE/env can setup your java.library.path to point at 
		// the right native libs (either src/main/resources/native/{platform} or the equiv directory under target/)

		JVisionLauncher jvl = new JVisionLauncher();
		jvl.attemptInit();
	}
	public JVisionLauncher() {
		myEngine = new JVisionEngine();
		myDemoFrame = new DemoFrame();
		myDemoFrame.setQuitter(this);
	}
	
	public boolean  attemptInit() {
		boolean connectedOK = myEngine.connect();
		if (connectedOK) {
			FilterSequence fseq = myEngine.getFilterSeq();
			myDemoFrame.setControlledFilterSequence(fseq);
			myEngine.setDisplayer(myDemoFrame);
			myEngine.setQuitter(this);
			return startThread();
		} else {
			return false;
		}
	}
	
	private boolean startThread() { 
		myCamProcThread = new Thread(myEngine);
		myCamProcThread.start();		
		return true;
	}
	public void requestStop() { 
		setWantsToQuit(true);
	}
	public void forceStop() { 
		myCamProcThread.interrupt();
	}

	@Override public boolean wantsToQuit()
	{
		synchronized(myQuittingFlag)
		{
			return myQuittingFlag;
		}
	}
	
	@Override public void setWantsToQuit(boolean x)
	{
		synchronized(myQuittingFlag)
		{
			myQuittingFlag = new Boolean(x);
		}
		
	}
	

}
