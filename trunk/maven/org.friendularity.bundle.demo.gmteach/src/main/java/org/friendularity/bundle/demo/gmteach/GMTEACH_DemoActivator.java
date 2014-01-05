package org.friendularity.bundle.demo.gmteach;

import org.appdapter.osgi.core.BundleActivatorBase;
import org.friendularity.bundle.macro.jvision.*;
import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;
import org.osgi.framework.FrameworkEvent;
import org.osgi.framework.FrameworkListener;

/**
 * This class is a bundle activator demonstrating how to start the Cogchar PUMA system, in an OSGi environment. We call such a bundle a "top" bundle, which is essentially a launchable application. This bundle can be launched using a maven command line like:
 * 
 * mvn -Prun-on-felix antrun:run
 * 
 * ...which is equivalent to a project->run command in Netbeans or Eclipse.
 * 
 * When the bundle is loaded (in activator "start"), it activates Log4J logging using forceLog4jConfig(), which assumes a "log4j.properties" file-resource is available on the classpath.
 * 
 * In principle, this bundle should start any desired subset of Cogchar functionality, for *any* environment, including
 * <ul>
 * <li>Cogchar behavior</li>
 * <li>Robokind animation and speech</li>
 * <li>Optional Cogchar OpenGL rendering</li>
 * </ul>
 * 
 * The actual subset is determined through the mediation of a customizable PumaContextMediator object.
 * 
 * L1) This bundle does not try to start a lifter webapp, which is currently initialized orthogonally to the PUMA system. See the o.f.b.demo.liftoff project.
 * 
 * To exit, a user may X-closes our "main" simulator window, which calls stop on bundle 0.
 * 
 * @author Stu B. <www.texpedient.com>
 */
public class GMTEACH_DemoActivator //
		//extends CCRK_DemoActivator //
		extends LikeSuperActivator // 
		// extends BMDActivatorSpecTest // 
		//extends BMDSpecActivator // 
		//
		implements BundleActivator, FrameworkListener {

	BundleActivatorBase bundleActivatorBase = null;
	private boolean booleanStart = true;

	public GMTEACH_DemoActivator() {
		//bundleActivatorBase = new CCRK_DemoActivator();
		booleanStart = true;
	}

	@Override public void frameworkEvent(FrameworkEvent event) {
		super.frameworkEvent(event);
		if (bundleActivatorBase != null && bundleActivatorBase != this)
			bundleActivatorBase.frameworkEvent(event);
	}

	public void start(BundleContext bundleCtx) throws Exception {
		// TODO Auto-generated method stub
		if (booleanStart)
			super.start(bundleCtx);
		if (bundleActivatorBase != null && bundleActivatorBase != this)
			bundleActivatorBase.start(bundleCtx);
	}

	@Override public void stop(BundleContext context) throws Exception {
		if (booleanStart)
			super.stop(context);
		if (bundleActivatorBase != null && bundleActivatorBase != this)
			bundleActivatorBase.stop(context);
	}

}
