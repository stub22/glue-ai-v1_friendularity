package org.friendularity.bundle.demo.ccrk;

import org.appdapter.osgi.core.BundleActivatorBase;

import org.cogchar.bundle.app.puma.PumaAppContext;
import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;


public class CCRK_DemoActivator extends BundleActivatorBase {

	@Override public void start(BundleContext context) throws Exception {
		// Will look for log4j.properties at root of this bundle.
		// Any top-level OSGi app that wants to enable Log4J (and thereby make Jena happy, while
		// retaining the power to configure Jena's logging level) should have the dependencies
		// in our pom.xml, and call this once at startup.
		forceLog4jConfig();
		// Print some howdys
		super.start(context);
		// Start the basic PUMA application (Cogchar OpenGL rendering with Robokind animation and speech).
		startPumaDemo(context);
		// Could start some extra app-specific (e.g.) Cogbot binding goodies, tell them to attach to PUMA behavior system.
	}
    @Override public void stop(BundleContext context) throws Exception {
		super.stop(context);
    }
	
	protected void startPumaDemo(BundleContext bundleCtx) throws Exception {
		String uriPrefix = "http://model.cogchar.org/char/bony/";
		String bonyCharUniqueSuffix = "0x0000FFFF";
		String sysContextURI = uriPrefix + bonyCharUniqueSuffix;
		String debugTxt = "sysContextURI = [" + sysContextURI + "]";
		logInfo("======================================== Starting " + debugTxt);		
		PumaAppContext pac = new PumaAppContext(bundleCtx, sysContextURI, null);
		try {
			pac.makeDualCharsForSwingOSGi();
		} catch (Throwable t) {
			logError("Cannot initialize " + debugTxt, t);
		}
		
		logInfo("Started" + debugTxt + "\n========================================");		
	}

}
