package org.friendularity.bundle.demo.ccrk;

import org.appdapter.osgi.core.BundleActivatorBase;

import org.cogchar.bundle.app.puma.PumaAppContext;
import org.cogchar.bundle.app.puma.PumaBooter;
import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;
import org.osgi.framework.FrameworkEvent;
import org.osgi.framework.FrameworkListener;
import org.slf4j.LoggerFactory;


public class CCRK_DemoActivator extends BundleActivatorBase {

	@Override public void start(final BundleContext context) throws Exception {
		// Will look for log4j.properties at root of this bundle.
		// Any top-level OSGi app that wants to enable Log4J (and thereby make Jena happy, while
		// retaining the power to configure Jena's logging level) should have the dependencies
		// in our pom.xml, and call this once at startup.
		forceLog4jConfig();
		// Print some howdys
		super.start(context);
		context.addFrameworkListener(new FrameworkListener() {
			public void frameworkEvent(FrameworkEvent fe) {
				int eventType = fe.getType();
				logInfo("************************ Got frameworkEvent with eventType=" + eventType + ", bundle=" + fe.getBundle());
				if (eventType == FrameworkEvent.STARTED) {
					logInfo("********  OSGi Framework has STARTED, we are ready for the pumaDemo!");
					startPumaDemo(context);
				}
			}
		});		
		
		// Here we could start some extra app-specific (e.g.) Cogbot binding goodies, tell them to attach to PUMA 
		// behavior system.  However, the Cogchar config system is intended to be sufficiently general to
		// handle most initialization cases without help from bundle activators.
	}
    @Override public void stop(BundleContext context) throws Exception {
		super.stop(context);
    }
/**
 * 	Start the PUMA application (Cogchar OpenGL rendering and behavior with Robokind animation and speech).
 * @param bundleCtx
 */	
	protected void startPumaDemo(BundleContext bundleCtx) {
		PumaBooter pumaBooter = new PumaBooter();
		PumaBooter.BootResult bootResult = pumaBooter.bootUnderOSGi(bundleCtx);
		logInfo("PUMA BootResult: " + bootResult);
	}

}
