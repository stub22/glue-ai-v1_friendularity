package org.friendularity.bundle.blockflow;

import org.friendularity.bundle.blockflow.gui.BlockflowLauncher;
import org.appdapter.osgi.core.BundleActivatorBase;
import org.osgi.framework.BundleContext;

/**
 * Needed link object for OSGi
 * 
 * @author Annie
 */
public class BlockflowBundleActivator extends BundleActivatorBase {
	
	BlockflowLauncher myLauncher;

    @Override public void start(BundleContext context) throws Exception {
		// We assume another bundle (e.g. JVision) will configure the SLF4J+Log4J logging system.
		// Print some howdys
		super.start(context);		
		getLogger().info("BlockFlow bundle postponing initialization until other OSGi bundles fully started.");
		scheduleFrameworkStartEventHandler(context);
		
    }

    @Override public void stop(BundleContext context) throws Exception {
		// This handler is important in the case where *some other bundle* (outside JVision) is trying
		// to stop the JVM/OSGi process.  
		if (myLauncher != null) {
			getLogger().info("Activator sending requestStop() to myLauncher, in case some *other* bundle is stopping the container.");
			myLauncher.requestStop(Boolean.FALSE);
		}
		getLogger().info("BlockFlow bundle dies!");
		// Print final regrets
		super.stop(context);
    }
		
	@Override
	protected void handleFrameworkStartedEvent(BundleContext bundleCtx) {
		// SplashScreen.getSplashScreen();
		
		getLogger().info("BlockFlow bundle is notified that all OSGi bundles have started.");		
		getLogger().info("Let the plumbing commence!!!");

		boolean flag_stopOSGiAfterQuitCompletes = true;
		myLauncher = new BlockflowLauncher(flag_stopOSGiAfterQuitCompletes);
		boolean launchedOK = myLauncher.attemptInit();
		getLogger().info("myLauncher.attemptInit() returned: " + launchedOK);
	}
}
