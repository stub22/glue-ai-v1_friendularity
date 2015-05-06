package org.friendularity.bundle.demo.liftoff;

import org.appdapter.osgi.core.BundleActivatorBase;
import org.cogchar.app.puma.web.LifterLifecycle;
import org.cogchar.bundle.app.puma.GruesomeTAProcessingFuncs;

import org.osgi.framework.BundleContext;
import org.jflux.impl.services.rk.osgi.lifecycle.OSGiComponent;
// import org.cogchar.joswrap.RepoUpdateCallbackAdapter;

public class Activator extends BundleActivatorBase {

    public void start(BundleContext context) throws Exception {
        initWebapp(context);
		setupRepoUpdateCallback();
    }

    public void stop(BundleContext context) throws Exception {
        // TODO add deactivation code here
    }

    public void initWebapp(BundleContext context) {
        // Tell the lifter lifecycle to start, once its dependencies are satisfied
        LifterLifecycle lifecycle = new LifterLifecycle();
        OSGiComponent lifterComp = new OSGiComponent(context, lifecycle);
        lifterComp.start();
    }

	public void setupRepoUpdateCallback() {
		/**
		 * This is a giant hack around the topic of properly notifying our VWorld (or any other aspect of Cogchar)
		 * that something in our "main memory" repo has changed.  At present we assume that any such update means
		 * we should simply update the Goodies in our V-World.
		 */
		
/*******
 Disabled 2015-05-05
		RepoUpdateCallbackAdapter.registerCallback(new RepoUpdateCallbackAdapter.Callback() {
			public void repoUpdateCompleted() {
				getLogger().info("o.f.b.demo.liftoff activator got SPARQL-UPDATE callback, now pumping Goody Updates!");
				GruesomeTAProcessingFuncs.processPendingThingActions();
			}
		});
		*/
	}
}
