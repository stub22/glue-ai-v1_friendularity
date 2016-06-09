package org.friendularity.old.ccmio;

import org.appdapter.core.log.BasicDebugger;
import org.appdapter.fancy.rspec.RepoSpec;
import org.appdapter.xload.rspec.OnlineSheetRepoSpec;
import org.cogchar.app.puma.boot.PumaBooter;
import org.cogchar.app.puma.boot.PumaSysCtx;
import org.cogchar.app.puma.config.PumaContextMediator;
import org.cogchar.app.puma.registry.PumaGlobalPrebootInjector;
import org.cogchar.bundle.app.vworld.central.VirtualWorldFactory;
import org.osgi.framework.BundleContext;

/**
 * Created by Owner on 6/9/2016.
 */
public class OldLaunchHelper extends BasicDebugger {
	public void startOldPumaThenVWorld(BundleContext bundleCtx) {
		getLogger().info("============ Calling launchPumaRobotsAndChars()  ==========");
		launchPumaRobotsAndChars(bundleCtx);
		getLogger().info("============ Calling launchCogcharVWorldLifecycles() ========");
		launchVWorldLifecycles(bundleCtx);

	}
	private void launchPumaRobotsAndChars(BundleContext bundleCtx) {
		PumaBooter pumaBooter = new PumaBooter();
		// Older config mechanism is used only if attachVizTChunk repo is *not* used.
		PumaContextMediator mediator = PumaGlobalPrebootInjector.getTheInjector().getMediator();

		// PumaBooter no longer includes the V-World.
		// Sets up character config mappings in a way we are now ready to fixup (Stu - 2016-03-16)
		PumaBooter.BootResult bootResult = pumaBooter.bootUnderOSGi(bundleCtx, mediator);
		getLogger().info("Got PUMA BootResult: " + bootResult);
	}
	private void launchVWorldLifecycles(BundleContext bundleCtx) {
		launchCogcharVWorldLifecycles(bundleCtx);
		OldVWorldHelperLifecycle.startHelperLifecycle(bundleCtx);
		// Last checked (2014) this lifecycle-start appears to actually launch the VWorld inline,
		// on this same thread, as shown by stack trace below.
/*
 *   [java] 	at org.cogchar.bundle.app.vworld.startup.PumaVirtualWorldMapper.initCinematicStuff(PumaVirtualWorldMapper.java:165)
     [java] 	at org.cogchar.bundle.app.vworld.startup.PumaVirtualWorldMapper.initVirtualWorlds(PumaVirtualWorldMapper.java:115)
     [java] 	at org.cogchar.bundle.app.vworld.central.VWorldRegistry.initCinema(VWorldRegistry.java:117)
     [java] 	at org.cogchar.bundle.app.vworld.central.VWorldMapperLifecycle.createService(VWorldMapperLifecycle.java:100)
     [java] 	at org.cogchar.bundle.app.vworld.central.VWorldMapperLifecycle.createService(VWorldMapperLifecycle.java:28)
     [java] 	at org.jflux.api.service.ServiceManager.tryCreate(ServiceManager.java:184)
     [java] 	at org.jflux.api.service.ServiceManager.bindDependencies(ServiceManager.java:162)
     [java] 	at org.jflux.api.service.ServiceManager.start(ServiceManager.java:128)
     [java] 	at org.cogchar.bundle.app.vworld.central.VirtualWorldFactory.startVWorldLifecycle(VirtualWorldFactory.java:73)
     [java] 	at org.friendularity.old.ccmio.OldVWorldHelper.launchCogcharVWorldLifecycles(OldVWorldHelper.java:118)
    ...
     [java] 	at org.friendularity.bundle.demo.ccmio.CCMIO_DemoActivator.handleFrameworkStartedEvent(CCMIO_DemoActivator.java:129)

 */
	}
	private void launchCogcharVWorldLifecycles(BundleContext bundleCtx) {
		// The startVWorldLifecycle call is only necessary under new-PUMA regime.
		// Starts the lifecycle for Cogchar VWorldRegistry, which has 5 input dependencies.
		getLogger().info("Starting Cogchar VWorldLifecycle using bundleContext {}", bundleCtx);
		VirtualWorldFactory.startVWorldLifecycle(bundleCtx);
	}
	@Deprecated  public void registerOldMediatorStuff_duringStart() {
		// Register a dummySheet default mediator, which only acts if there is no vizapp-tchunk repo.
		// (Reads from online sheet and functions well as of 2016-03-19, with vizapp-tchunk-flag == false)
		DummySheetMediator mediator = new DummySheetMediator();
		PumaGlobalPrebootInjector injector = PumaGlobalPrebootInjector.getTheInjector();
		// False => Do not overwrite, so any other customer mediator will get preference.
		// (Crude DemoMediator coded at bottom of file is only used as a backup/default).
		injector.setMediator(mediator, false);

	}

	// These mediators decorate the application lifecycle as needed.
	// This early example shows a hardcoded reference to a particular online spreadsheet config.
	// It only has effect if there is no other config source set above (e.g. see VizTChunk)
	private static class DummySheetMediator extends PumaContextMediator {
		// Override base class methods to customize the way that PUMA boots + runs, and
		// to receive notifications of progress during the boot / re-boot process.
		String TEST_REPO_SHEET_KEY = "0ArBjkBoH40tndDdsVEVHZXhVRHFETTB5MGhGcWFmeGc";
		int  DFLT_NAMESPACE_SHEET_NUM = 9;
		int   DFLT_DIRECTORY_SHEET_NUM = 8;

		public PumaSysCtx myDemoPACtx;

		@Override public RepoSpec getMainConfigRepoSpec() {
			java.util.List<ClassLoader> fileResModelCLs = new java.util.ArrayList<ClassLoader>();
			return new OnlineSheetRepoSpec(TEST_REPO_SHEET_KEY, DFLT_NAMESPACE_SHEET_NUM, DFLT_DIRECTORY_SHEET_NUM,
					fileResModelCLs);
		}
		@Override public void notifyBeforeBootComplete(PumaSysCtx ctx) throws Throwable {
			myDemoPACtx = ctx;
			// We could do some additional init here, if desired.
			// We are on the frameworkStartedCallback() thread.
		}
	}

}
