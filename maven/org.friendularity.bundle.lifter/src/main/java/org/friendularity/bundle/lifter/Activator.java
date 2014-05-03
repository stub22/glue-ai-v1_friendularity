package org.friendularity.bundle.lifter;


import org.appdapter.osgi.core.BundleActivatorBase;
// import org.cogchar.app.puma.web.MockWebAppLauncher;
import org.cogchar.impl.netconf.CogCharNetworkServiceLauncher;

import org.osgi.framework.BundleContext;

	
public class Activator extends BundleActivatorBase {

	// public static Dataset theMainConfigDataset;
	
	public void start(BundleContext context) throws Exception {
		// Lift calls bootstrap.liftweb.Boot.boot before this -
		// as a result of classloading...triggered during PAX processing of web.xml?
		forceLog4jConfig0();
		doLaunch(context);
	}

	public void stop(BundleContext context) throws Exception {
		// TODO add deactivation code here
	}

	private void doLaunch(BundleContext context) {
		CogCharNetworkServiceLauncher.startCogCharAmbassadors(context);
//		getLogger().info("LiftRules.buildPackage(yowza)=" + LiftRules.realInstance().buildPackage("yowza"));
//		getLogger().info("Calling PUMA's MockWebAppLauncher.initWebapps");
		
		// MockWebAppLauncher mapl = new MockWebAppLauncher();
		// mapl.initWebapps(context);
//		getLogger().info("LiftRules.buildPackage(nebbish)=" + LiftRules.realInstance().buildPackage("nebbish"));

	}	
}
