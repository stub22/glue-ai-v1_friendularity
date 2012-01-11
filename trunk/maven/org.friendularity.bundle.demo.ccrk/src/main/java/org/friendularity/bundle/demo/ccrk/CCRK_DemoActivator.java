package org.friendularity.bundle.demo.ccrk;

import org.appdapter.osgi.core.BundleActivatorBase;

import org.cogchar.bundle.app.puma.PumaAppContext;
import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class CCRK_DemoActivator extends BundleActivatorBase {
	static Logger theLogger = LoggerFactory.getLogger(CCRK_DemoActivator.class);
	@Override protected Logger getLogger() {
		return theLogger;
	}	
    @Override public void start(BundleContext context) throws Exception {
		// Will look for log4j.properties at root of this bundle.
		forceLog4jConfig();
		// Print a bunch of howdys
		super.start(context);
		
		startPumaDemo(context);
	}
    @Override public void stop(BundleContext context) throws Exception {
		super.stop(context);
    }
	
	protected void startPumaDemo(BundleContext bundleCtx) throws Exception {
		String uriPrefix = "http://model.cogchar.org/char/bony/";
		String bonyCharUniqueSuffix = "0x0000FFFF";
		String bonyCharURI = "http://model.cogchar.org/char/bony/" + bonyCharUniqueSuffix;
		String debugTxt = "bonyChar at URI[" + bonyCharURI + "]";
		theLogger.info("======================================== Starting " + debugTxt);		
		PumaAppContext pac = new PumaAppContext(bundleCtx);
		try {
			pac.makeDualCharForSwingOSGi(bonyCharURI);
		} catch (Throwable t) {
			theLogger.error("Cannot initialize " + debugTxt, t);
		}
		
		theLogger.info("Started" + debugTxt + "\n========================================");		
	}

}
