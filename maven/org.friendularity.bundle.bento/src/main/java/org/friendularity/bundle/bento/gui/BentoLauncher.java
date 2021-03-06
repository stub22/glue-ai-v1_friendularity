package org.friendularity.bundle.bento.gui;

import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.util.ArrayList;
import org.appdapter.core.log.BasicDebugger;
import org.friendularity.bundle.bento.engine.Quitter;
import org.friendularity.jvision.engine.JVisionEngine;

import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;

public class BentoLauncher extends BasicDebugger implements Quitter, WindowListener {

	private Boolean			myFlag_QuittingNow  = Boolean.FALSE;	
	private	boolean			myFlag_StopOSGiAfterQuit = false;
	
	private static BentoLauncher defBentoLauncher = null;
	
	private ArrayList<BentoFrame> bframes = new ArrayList<BentoFrame>();
	
	public static BentoLauncher getDefaultLauncher() {
		if (defBentoLauncher == null)
		{
			defBentoLauncher = new BentoLauncher(false);
		}
		return defBentoLauncher;
	}

	public static void main(String[] args) {
		// Can use this to run-file without bundling, if your IDE/env can setup your java.library.path to point at 
		// the right native libs (either src/main/resources/native/{platform} or the equiv directory under target/)

		getDefaultLauncher().attemptInit();
	}
	
	public BentoLauncher(boolean flag_stopOSGiAfterQuit) {
		myFlag_StopOSGiAfterQuit = flag_stopOSGiAfterQuit;
	}
	
	public boolean  attemptInit() {
		JVisionEngine.getDefaultJVisionEngine();  // starts the engine
		
		addWindow(new BentoFrame());
		return true;
	}

	void addWindow(BentoFrame bf) {
		bframes.add(bf);
		bf.addWindowListener(this);
	}
	
	public void requestStop(Boolean optionalFlag_stopOSGiAfterQuit) { 
		if (optionalFlag_stopOSGiAfterQuit != null) {
			myFlag_StopOSGiAfterQuit = optionalFlag_stopOSGiAfterQuit;
		}
		// This may have already been done, that's OK.
		setWantsToQuit(true);
	}
	
	@Override 
	public boolean wantsToQuit()
	{
		synchronized(myFlag_QuittingNow)
		{
			return myFlag_QuittingNow;
		}
	}
	
	@Override public void setWantsToQuit(boolean x)
	{
		synchronized(myFlag_QuittingNow)
		{
			myFlag_QuittingNow = new Boolean(x);
		}
		
	}
	@Override public void notifyQuitCompleted() {
		if (myFlag_StopOSGiAfterQuit) { 
			getLogger().info("Now that quit has completed, we will shutdown our OSGi container");
			shutdownOurOSGiContainer();
		}
	}
	private void shutdownOurOSGiContainer() { 
		Bundle anyB = org.osgi.framework.FrameworkUtil.getBundle(getClass());
		BundleContext anyBC = anyB.getBundleContext();
		stopOSGiContainer(anyBC);
	}
	
	private void stopOSGiContainer(BundleContext bc) { 
		Bundle sysB = bc.getBundle(0);
		getLogger().warn("Asking system bundle to stop(): {}", sysB);
		try {
			sysB.stop();
		} catch (Throwable t) {
			getLogger().error("Caught exception during sys-bundle.stop() request", t);
		}
	}

	@Override
	public void windowOpened(WindowEvent e) {
		
	}

	@Override
	public void windowClosing(WindowEvent e) {
		
	}

	@Override
	public void windowClosed(WindowEvent e) {
		if(bframes.indexOf(e.getComponent()) >= 0)
		{
			bframes.remove(e.getComponent());
		}
		if(bframes.size() == 0)
			setWantsToQuit(true);
	}

	@Override
	public void windowIconified(WindowEvent e) {
		
	}

	@Override
	public void windowDeiconified(WindowEvent e) {
		
	}

	@Override
	public void windowActivated(WindowEvent e) {
		
	}

	@Override
	public void windowDeactivated(WindowEvent e) {
		
	}
}
