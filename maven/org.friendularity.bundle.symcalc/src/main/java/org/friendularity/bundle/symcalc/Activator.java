package org.friendularity.bundle.symcalc;

import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;

public class Activator implements BundleActivator {

    public void start(BundleContext context) throws Exception {
        ScriptEngineExperiment.main(null);
		System.out.println("***********************************************");
		System.out.println("***********************************************");
		System.out.println("You need to kill this process now!");
    }

    public void stop(BundleContext context) throws Exception {
        // TODO add deactivation code here
    }

}
