package org.friendularity.bundle.repo;

import org.appdapter.core.name.FreeIdent;
import org.appdapter.core.name.Ident;
import org.appdapter.osgi.core.BundleActivatorBase;
import org.cogchar.bind.lift.LifterLifecycle;
import org.cogchar.blob.emit.OnlineSheetRepoSpec;
import org.cogchar.blob.emit.RepoSpec;
import org.cogchar.app.puma.boot.PumaAppContext;
import org.cogchar.app.puma.boot.PumaContextCommandBox;
import org.cogchar.app.puma.cgchr.PumaWebMapper;
import org.cogchar.app.puma.config.PumaConfigManager;
import org.cogchar.app.puma.config.PumaContextMediator;
import org.cogchar.app.puma.config.PumaModeConstants;
import org.osgi.framework.BundleContext;
import org.robokind.api.common.osgi.lifecycle.OSGiComponent;

import org.appdapter.help.repo.RepoClient;
import org.appdapter.core.store.Repo;
import com.hp.hpl.jena.query.Dataset;

public class Activator extends BundleActivatorBase {

	public static Dataset theMainConfigDataset;
	
	public void start(BundleContext context) throws Exception {
		forceLog4jConfig();
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
		PumaWebMapper pwm = pac.getOrMakeWebMapper();	
		pwm.connectLiftInterface(context);
		// Tell the lifter lifecycle to start, once its dependencies are satisfied
		LifterLifecycle lifecycle = new LifterLifecycle();
    	OSGiComponent lifterComp = new OSGiComponent(context, lifecycle);
    	lifterComp.start();
		
		PumaContextCommandBox pccb = pwm.getCommandBox();
		RepoClient mainConfRC = pccb.getMainConfigRepoClient();
		Repo mainConfRepo = mainConfRC.getRepo();
		Dataset mainConfDset = mainConfRepo.getMainQueryDataset();
		theMainConfigDataset = mainConfDset;
		
		java.util.List<Repo.GraphStat> gStats = mainConfRepo.getGraphStats();
		for (Repo.GraphStat gStat : gStats) {
			System.out.println("Found in main config:  " + gStat);
		}
	}
	
	private static class RepoPumaMediator extends PumaContextMediator {
		// Override base class methods to customize the way that PUMA boots + runs, and
		// to receive notifications of progress during the boot / re-boot process.
		String TEST_REPO_SHEET_KEY = "0ArBjkBoH40tndDdsVEVHZXhVRHFETTB5MGhGcWFmeGc"; // Main Repo
		//String TEST_REPO_SHEET_KEY = "0Ajj1Rnx7FCoHdDN2VFdVazMzRGNGY3BMQmk1TXZzUHc"; // Biggs Test Repo
		int  DFLT_NAMESPACE_SHEET_NUM = 9;
		int   DFLT_DIRECTORY_SHEET_NUM = 8;
		
		@Override public RepoSpec getMainConfigRepoSpec() {
			java.util.List<ClassLoader> fileResModelCLs = new java.util.ArrayList<ClassLoader>();
			return new OnlineSheetRepoSpec(TEST_REPO_SHEET_KEY, DFLT_NAMESPACE_SHEET_NUM, DFLT_DIRECTORY_SHEET_NUM,
							fileResModelCLs);
		}
	}
}
