package org.friendularity.bundle.macro.jvision;

import org.appdapter.core.matdat.OnlineSheetRepoSpec;
import org.appdapter.core.matdat.RepoSpec;
import org.cogchar.app.puma.boot.PumaAppContext;
import org.cogchar.app.puma.config.PumaContextMediator;

// These mediators decorate the application lifecycle as needed.
public class CCRK_DemoMediator extends PumaContextMediator {
	// Override base class methods to customize the way that PUMA boots + runs, and
	// to receive notifications of progress during the boot / re-boot process.
	String TEST_REPO_SHEET_KEY = "0ArBjkBoH40tndDdsVEVHZXhVRHFETTB5MGhGcWFmeGc";
	int DFLT_NAMESPACE_SHEET_NUM = 9;
	int DFLT_DIRECTORY_SHEET_NUM = 8;

	public PumaAppContext myDemoPACtx;

	@Override public RepoSpec getMainConfigRepoSpec() {
		java.util.List<ClassLoader> fileResModelCLs = new java.util.ArrayList<ClassLoader>();
		return new OnlineSheetRepoSpec(TEST_REPO_SHEET_KEY, DFLT_NAMESPACE_SHEET_NUM, DFLT_DIRECTORY_SHEET_NUM,
				fileResModelCLs);
	}

	@Override public void notifyBeforeBootComplete(PumaAppContext ctx) throws Throwable {
		myDemoPACtx = ctx;
		// We could do some additional init here, if desired.
	}
}