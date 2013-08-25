package org.friendularity.bundle.blockflow;

import org.appdapter.gui.demo.DemoBrowser;
import org.appdapter.osgi.core.BundleActivatorBase;
import org.friendularity.jvision.gui.JVisionLauncher;
import org.friendularity.math.api.MathGate;
import org.friendularity.math.api.MathSpaceFactory;
import org.matheclipse.core.interfaces.IExpr;
import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;

public class BlockflowBundleActivator extends BundleActivatorBase {

    @Override public void start(BundleContext context) throws Exception {
		// We assume another bundle (e.g. JVision) will configure the SLF4J+Log4J logging system.
		
		// Print some howdys
		super.start(context);		
		getLogger().info("BlockFlow bundle postponing initialization until other OSGi bundles fully started.");
		scheduleFrameworkStartEventHandler(context);
    }

    @Override public void stop(BundleContext context) throws Exception {
		getLogger().info("BlockFlow bundle dies!");
		// Print final regrets
		super.stop(context);
    }
	@Override protected void handleFrameworkStartedEvent(BundleContext bundleCtx) {
		getLogger().info("BlockFlow bundle is notified that all OSGi bundles have started.");		
		getLogger().info("Initializing a MathSpaceFactory, and a first mathGate");
		MathSpaceFactory msf = new MathSpaceFactory();
		MathGate firstMathgate = msf.makeUnscriptedMathGate();
		getLogger().info("(unnecessarily!) launching an Appdapter-GUI debug window");
		DemoBrowser.showObject("blockflows-first-mathgate", firstMathgate, false, false); 
		String sillyVecExprText = "17 * {-4.0, 3, -2 / 5, 1.0}";
		// Will reuse a pre-parsed version if this input string has been seen before.
		IExpr sillySymbolicOutput = firstMathgate.parseAndEvalExprToIExpr(sillyVecExprText);
		getLogger().info("Evaluated [" + sillyVecExprText + "] to [" + sillySymbolicOutput + "]");
		
	}
}
