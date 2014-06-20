package org.friendularity.bundle.webapp.semrepo;

import org.appdapter.core.name.FreeIdent;
import org.appdapter.core.name.Ident;
import org.appdapter.osgi.core.BundleActivatorBase;
import org.appdapter.core.matdat.OnlineSheetRepoSpec;
import org.appdapter.core.repo.RepoSpec;
import org.cogchar.app.puma.boot.PumaAppContext;
import org.cogchar.app.puma.web.PumaWebMapper;
import org.cogchar.app.puma.config.PumaContextMediator;
import org.osgi.framework.BundleContext;
import com.hp.hpl.jena.query.Dataset;

import org.cogchar.joswrap.RepoUpdateCallbackAdapter;
import org.cogchar.name.dir.NamespaceDir;
import org.joseki.processors.ProcessorBase;

public class Activator extends BundleActivatorBase {

	public static Dataset theMainConfigDataset;
	
	public void start(BundleContext context) throws Exception {
	    /*
		forceLog4jConfig();
		initWebapps(context);
	    */

	}

	public void stop(BundleContext context) throws Exception {
		// TODO add deactivation code here
	}

	public void initWebapps(BundleContext context) {
		// Since we are not running PumaBooter, we must at least start the query service to get sheet-based config going
		PumaContextMediator mediator = new RepoPumaMediator();
		String roleShortName = "pumaCtx_FrienduRepo";
		Ident ctxID = new FreeIdent(NamespaceDir.RKRT_NS_PREFIX + roleShortName, roleShortName);		
		PumaAppContext pac = new PumaAppContext(context, mediator, ctxID);
		pac.startRepositoryConfigServices();
		// ... and set our app context with PumaWebMapper, so lift can issue repo update requests
		PumaWebMapper pwm = pac.getOrMakeWebMapper();	
		pwm.connectLiftInterface(context);
		// Tell the lifter lifecycle to start, once its OSGi dependencies are satisfied
		pwm.startLifterLifecycle(context);
		setupJosekiSparqlAccess(pwm);
	}
	protected void setupJosekiSparqlAccess(PumaWebMapper pwm) { 
		// First stab at connecting outer code to our config dataset uses this ugly static variable.  
		theMainConfigDataset = pwm.getMainSparqlDataset();
		RepoUpdateCallbackAdapter.registerCallback(new RepoUpdateCallbackAdapter.Callback() {
			public void repoUpdateCompleted() {
				getLogger().info("o.f.b.repo activator got SPARQL-UPDATE callback");
				// System.out.println("***\n*** (System.out direct) \n***\nYep, got repo SPARQL-UPDATE callback!!!");
			}
		});
		//ProcessorBase.stusLockingEnabledFlag = false;
		//ProcessorBase.stusTransactEnabledFlag = false;
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

