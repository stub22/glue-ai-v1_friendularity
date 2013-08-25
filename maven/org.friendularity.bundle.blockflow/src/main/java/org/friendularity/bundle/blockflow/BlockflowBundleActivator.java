package org.friendularity.bundle.blockflow;

import org.appdapter.osgi.core.BundleActivatorBase;
import org.friendularity.jvision.gui.JVisionLauncher;
import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;

public class BlockflowBundleActivator extends BundleActivatorBase {

    @Override public void start(BundleContext context) throws Exception {
		// We assume another bundle (e.g. JVision) will configure the SLF4J+Log4J logging system.
		
		// Print some howdys
		super.start(context);		
		getLogger().info("BlockFlow bundle lives!");
    }

    @Override public void stop(BundleContext context) throws Exception {
		getLogger().info("BlockFlow bundle dies!");
		// Print final regrets
		super.stop(context);
    }

}
