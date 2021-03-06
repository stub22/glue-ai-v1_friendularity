/**
 *
 * This is the missing license file
 *
 */
package org.friendularity.bundle.blockflow.gui;

import org.friendularity.bundle.blockflow.engine.BlockflowEngine;
import org.appdapter.core.log.BasicDebugger;
import org.friendularity.bundle.blockflow.engine.Quitter;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;

/**
 *
 * @author Annie
 */
public class BlockflowLauncher  extends BasicDebugger implements Quitter  {
	private BlockflowEngine myEngine;
	private BlockflowFrame  myFrame;
	private Boolean			myFlag_QuittingNow  = Boolean.FALSE;	
	private	boolean			myFlag_StopOSGiAfterQuit = false;
	
	public static void main(String[] args) {
		// Can use this to run-file without bundling, if your IDE/env can setup your java.library.path to point at 
		// the right native libs (either src/main/resources/native/{platform} or the equiv directory under target/)

		BlockflowLauncher bfl = new BlockflowLauncher(false);
		bfl.attemptInit();
	}
		
	public BlockflowLauncher(boolean flag_stopOSGiAfterQuitCompletes) {

		myFlag_StopOSGiAfterQuit = flag_stopOSGiAfterQuitCompletes;
	}

	
	@Override public boolean wantsToQuit()
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
	public boolean attemptInit() {
		myEngine = new BlockflowEngine();
		myFrame = new BlockflowFrame(myEngine);
		myFrame.setQuitter(this); 
		return true;
	}

	public void requestStop(Boolean optionalFlag_stopOSGiAfterQuit) { 
		if (optionalFlag_stopOSGiAfterQuit != null) {
			myFlag_StopOSGiAfterQuit = optionalFlag_stopOSGiAfterQuit;
		}
		// This may have already been done, that's OK.
		setWantsToQuit(true);
	}
	
}
