package org.friendularity.bundle.blockflow;

import java.util.Arrays;
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
		MathGate firstMG = msf.makeUnscriptedMathGate();
		getLogger().info("(unnecessarily!) launching an Appdapter-GUI debug window");
		// This one line causes the Appdapter-Whackamole GUI to display, with firstMG added to the browse tree.
		DemoBrowser.showObject("blockflows-first-mathgate", firstMG, false, false); 
		quickMathDemo(firstMG);

	}
	private void quickMathDemo(MathGate mg) { 
		// Let's make a vector math expression
		String sillyVecExprText = "$coeff * {-4.0, 3, -2 / 5, 1.0}";

		double valBuf[] = new double[4];
		for (int i = 0; i < 5 ; i++) {
			mg.putVar("$coeff", i);
			// Parse and evaluate.  MathGate instance will cache the parsed expression for future re-eval 
			IExpr sillySymbolicOutput = mg.parseAndEvalExprToIExpr(sillyVecExprText);			
			// This will eval the already parsed silly-expr again.
			// We could instead call firstMG.writeTreeResultIntoArray() to put the sillySymbolicOutput into a double array.
			mg.parseAndEvalExprToDoubleVec(sillyVecExprText, valBuf);

			getLogger().info("With $coeff=" + i + ", evaluated [" + sillyVecExprText + "] to symbolic [" + sillySymbolicOutput 
					+ "] and then to doubles [" + Arrays.toString(valBuf) + "]");
		}		
	}
}
