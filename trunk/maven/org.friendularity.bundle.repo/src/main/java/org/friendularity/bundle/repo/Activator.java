package org.friendularity.bundle.repo;

import org.appdapter.core.name.FreeIdent;
import org.appdapter.core.name.Ident;
import org.cogchar.bind.lift.LifterLifecycle;
import org.cogchar.bundle.app.puma.PumaAppContext;
import org.cogchar.bundle.app.puma.PumaModeConstants;
import org.cogchar.bundle.app.puma.PumaContextMediator;

import org.cogchar.blob.emit.RepoSpec;
import org.cogchar.blob.emit.OnlineSheetRepoSpec;

import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;
import org.robokind.api.common.osgi.lifecycle.OSGiComponent;


public class Activator implements BundleActivator {

	public void start(BundleContext context) throws Exception {
		initWebapp(context);
	}

	public void stop(BundleContext context) throws Exception {
		// TODO add deactivation code here
	}

	public void initWebapp(BundleContext context) {
		// Since we are not running PumaBooter, we must at least start the query service to get sheet-based config going
		PumaContextMediator mediator = new RepoPumaMediator();
		String roleShortName = "pumaCtx_FrienduRepo";
		Ident ctxID = new FreeIdent(PumaModeConstants.RKRT_NS_PREFIX + roleShortName, roleShortName);		
		PumaAppContext pac = new PumaAppContext(context, mediator, ctxID);
		pac.startRepositoryConfigServices();
		// ... and set our app context with PumaWebMapper, so lift can issue repo update requests
		pac.getOrMakeWebMapper().connectLiftInterface(context);
		// Tell the lifter lifecycle to start, once its dependencies are satisfied
		LifterLifecycle lifecycle = new LifterLifecycle();
    	OSGiComponent lifterComp = new OSGiComponent(context, lifecycle);
    	lifterComp.start();
	}
	
	private static class RepoPumaMediator extends PumaContextMediator {
		// Override base class methods to customize the way that PUMA boots + runs, and
		// to receive notifications of progress during the boot / re-boot process.
		String TEST_REPO_SHEET_KEY = "0ArBjkBoH40tndDdsVEVHZXhVRHFETTB5MGhGcWFmeGc";
		int  DFLT_NAMESPACE_SHEET_NUM = 9;
		int   DFLT_DIRECTORY_SHEET_NUM = 8;
		
		@Override public RepoSpec getMainConfigRepoSpec() {
			return new OnlineSheetRepoSpec(TEST_REPO_SHEET_KEY, DFLT_NAMESPACE_SHEET_NUM, DFLT_DIRECTORY_SHEET_NUM);
		}
	}
}
