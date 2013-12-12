package org.cogchar.bundle.app.puma;

import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;

import org.appdapter.osgi.core.BundleActivatorBase;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * The PumaAppBundleActivator does nothing but print some log messages when it is started or stopped.
 * TODO:  Make the "stop()" method do appropriate cleanup.
 * 
 * @author Stu B. <www.texpedient.com>
 */
public class PumaAppBundleActivator extends BundleActivatorBase {

	static Logger theLogger = LoggerFactory.getLogger(PumaAppBundleActivator.class);
	
	@Override protected Logger getLogger() {
		return theLogger;
	}
    @Override public void start(BundleContext context) throws Exception {
		// We assume some other bundle has configured SLF4J for us.
		super.start(context);	
	}
	@Override public void stop(BundleContext context) throws Exception {
		super.stop(context);	
	}

}
