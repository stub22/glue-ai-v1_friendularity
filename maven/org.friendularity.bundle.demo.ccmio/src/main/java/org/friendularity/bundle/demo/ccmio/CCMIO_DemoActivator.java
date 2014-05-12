package org.friendularity.bundle.demo.ccmio;

import static ext.osgi.common.MacroBundleActivatorBase.macroStartupSettings;
import java.util.List;
import org.appdapter.osgi.core.BundleActivatorBase;

import org.cogchar.app.puma.boot.PumaBooter;
import org.cogchar.app.puma.config.PumaContextMediator;
import org.cogchar.app.puma.registry.PumaGlobalPrebootInjector;
import org.osgi.framework.BundleContext;

import org.appdapter.core.matdat.RepoSpec;
import org.appdapter.core.matdat.OnlineSheetRepoSpec;
import org.appdapter.core.name.FreeIdent;
import org.appdapter.core.name.Ident;
import org.appdapter.gui.demo.DemoBrowser;
import org.cogchar.app.puma.boot.PumaAppContext;
import org.cogchar.bind.mio.robot.motion.CogcharMotionSource;
import org.cogchar.bundle.app.puma.PumaAppUtils;
import org.cogchar.bind.symja.MathSpaceFactory;
import org.cogchar.bind.symja.MathGate;
import org.friendularity.api.west.WorldEstimate;

import org.friendularity.impl.visual.EstimateVisualizer;
// import org.cogchar.test.symcalc.ScriptEngineExperiment;
import org.friendularity.vworld.UnusedNetworkVisionDataFeed;
import org.mechio.api.motion.Robot;

// import org.mechio.ui.swing.common.lifecycle.ServicesFrame;
import org.rwshop.swing.common.lifecycle.ServicesFrame;
/**
 * This class is a bundle activator demonstrating how to start the Cogchar PUMA system, in an
 * OSGi environment.  We call such a bundle a "top" bundle, which is essentially a launchable
 * application.  This bundle can be launched using a maven command line like:
 * 
 * mvn -Prun-on-felix antrun:run
 * 
 * ...which is equivalent to a project->run command in Netbeans or Eclipse.
 * 
 * When the bundle is loaded (in activator "start"), it activates Log4J logging
 * using forceLog4jConfig(), which assumes a "log4j.properties" file-resource is
 * available on the classpath.
 *
 * In principle, this bundle should start any desired subset of Cogchar functionality, for *any* environment, including
 *		<ul><li>Cogchar behavior
 *		</li><li>MechIO animation and speech
 *		</li><li>Optional Cogchar OpenGL rendering</li></ul>

*  The actual subset is determined through the mediation of a customizable PumaContextMediator object.

* L1) This bundle does not try to start a lifter webapp, which is currently initialized
 * orthogonally to the PUMA system.  See the o.f.b.demo.liftoff project.
 * 		
 * To exit, a user may X-closes our "main" simulator window, which calls stop on bundle 0.
 * 
 * @author Stu B. <www.texpedient.com>
 */
public class CCMIO_DemoActivator extends BundleActivatorBase {

	/*
	 * Without JVision and the texture-mapping, memory usage hovers around 325-350M.
	 * With JVision, it cycles rapidly from about 500M to 800M.
	 * Part of this is the byte arrays alloced for each vision frame - but also the raw images, 
	 * and perhaps also the textures/materials on the V-World side? 
	 */
	
	public static	boolean		myFlag_connectJVision = true;  
	private	boolean		myFlag_connectObsoleteNetworkVision = false;  
	

	private	boolean		myFlag_connectSwingDebugGUI = true;
	private boolean		myFlag_monitorLifecycles = true;
	
	// private	CCMIO_VWorldHelper					myVWorldHelper;

	
	@Override public void start(final BundleContext context) throws Exception {
		// Need to tell the MacroBundle system that we are the main launcher, so that forceLog4JConfig will work.
		macroStartupSettings.firstBundleActivatorBase = this;
		// Will look for log4j.properties at root of this bundle.
		// Any top-level OSGi app that wants to enable Log4J (and thereby make Jena happy, while
		// retaining the power to configure Jena's logging level) should have the dependencies
		// in our pom.xml, and call this once at startup.
		forceLog4jConfig();
		// Print some howdys
		super.start(context);
		// Register our default mediator
		DemoMediator mediator = new DemoMediator();
		PumaGlobalPrebootInjector injector = PumaGlobalPrebootInjector.getTheInjector();
		// False => Do not overwrite, so any other customer mediator will get preference.
		// Our DemoMediator coded below is only used as a backup/default.
		injector.setMediator(mediator, false);
		// Schedule our callback to the handle method below.
		scheduleFrameworkStartEventHandler(context);
		if (myFlag_monitorLifecycles) {
			startLifecycleMonitorGuiWindow(context);
		}
		// New bugs in ScreenBoxImpl are preventing this window from launching.
		// startWhackamoleGuiWindow(context);
		
		// ScriptEngineExperiment.main(null);
		getLogger().info("Setting JVision.LAUNCH_MYSELF to {}", myFlag_connectJVision);
		org.friendularity.bundle.jvision.JVisionBundleActivator.setLaunchFlag(myFlag_connectJVision);

	}
	protected void startLifecycleMonitorGuiWindow(final BundleContext context) {
      
		ServicesFrame frame = new ServicesFrame();
		frame.setBundleContext(context);
		frame.setVisible(true);
  
       // SceneLifecycleDemo.test(context);		
/*  [java] java.lang.RuntimeException: Uri does not contain text after hash '#' [fakeURI]
     [java] 	at org.appdapter.core.name.FreeIdent.<init>(FreeIdent.java:40)
     [java] 	at org.cogchar.bind.rk.behavior.ChannelBindingConfig.initExplicitly(ChannelBindingConfig.java:41)
     [java] 	at org.cogchar.bind.rk.behavior.SceneLifecycleDemo.test(SceneLifecycleDemo.java:66)
     [java] 	at org.friendularity.bundle.demo.ccrk.CCRK_DemoActivator.startLifecycleMonitorGuiWindow(CCRK_DemoActivator.java:73)
     [java] 	at org.friendularity.bundle.demo.ccrk.CCRK_DemoActivator.start(CCRK_DemoActivator.java:63)
**/
	}
	/** Not currently needed
	protected void startWhackamoleGuiWindow(final BundleContext context) {
		org.cogchar.impl.trigger.Whackamole.launchWhackamoleGui(null);
	}
	*/
	@Override protected void handleFrameworkStartedEvent(BundleContext bundleCtx) {
		getLogger().info("Calling startPumaDemo()");
		
		launchPumaRobotsAndChars(bundleCtx);
		launchVWorldLifecycles(bundleCtx);
		launchOtherStuffLate();	
		// Here we *could start some extra app-specific (e.g. Cogbot binding) goodies, and tell them to attach to 
		// PUMA  behavior system.  However, the Cogchar config system is intended to be sufficiently general to
		// handle most initialization cases without help from bundle activators.		
	}
    @Override public void stop(BundleContext context) throws Exception {
		super.stop(context);
    }

	private void launchPumaRobotsAndChars(BundleContext bundleCtx) {
		PumaBooter pumaBooter = new PumaBooter();
		PumaContextMediator mediator = PumaGlobalPrebootInjector.getTheInjector().getMediator();

		// This part no longer includes the V-World.
		// N
		PumaBooter.BootResult bootResult = pumaBooter.bootUnderOSGi(bundleCtx, mediator);
		getLogger().info("Got PUMA BootResult: " + bootResult);
	}
	private void launchVWorldLifecycles(BundleContext bundleCtx) {
		CCMIO_VWorldHelper.launchVWorldLifecycles(bundleCtx);	
		CCMIO_VWorldHelperLifecycle.startHelperLifecycle(bundleCtx);
		// This seems to actually launch the VWorld inline, on this thread.
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
     [java] 	at org.friendularity.bundle.demo.ccmio.CCMIO_VWorldHelper.launchVWorldLifecycles(CCMIO_VWorldHelper.java:118)
    ...
     [java] 	at org.friendularity.bundle.demo.ccmio.CCMIO_DemoActivator.handleFrameworkStartedEvent(CCMIO_DemoActivator.java:129)

 */
	}
	private void launchOtherStuffLate()  {
	
		if (myFlag_connectObsoleteNetworkVision) {
			//	Startup the obsolete netrowk vision connection
			startObsoleteNetworkVisionMonitors();
		}
		if (myFlag_connectSwingDebugGUI) {
			// setupDebuggingScaffold(mg, we);
		}			
	}

	private void setupDebuggingScaffold(MathGate mg, WorldEstimate we) { 
	/*
		DemoBrowser.showObject("werm-MG", mg, false, false); // true, true);
		DemoBrowser.showObject("amazingly accurate estimate", we, false, false);
		PumaAppUtils.GreedyHandleSet greedyHandles = new PumaAppUtils.GreedyHandleSet();
		DemoBrowser.showObject("our-greedy-handles", greedyHandles, false, false);
		DemoBrowser.showObject("our-repo-client", greedyHandles.rc, false, false);
	*/
	}

	/**
	 * 		For each joint robot, mechio.org blends all joint inputs received from RobotMoverFrameSources.
	 *	Cogchar.org defines the CogcharMotionSource subclass.
	 *	A cogcharMotionSource has an ordered list of CogcharMotionComputers.
	 * @param bundleCtx
	 * @param optRobotID_elseAllRobots 
	 */

	private void startObsoleteNetworkVisionMonitors() { 
		UnusedNetworkVisionDataFeed vdf = new UnusedNetworkVisionDataFeed();
		boolean svcsOK = vdf.connectServices();
		getLogger().info("vdf.connectServices returned {}", svcsOK);
		if (svcsOK) {
			vdf.registerDummyListeners();
			vdf.startServices();
		}
	}

	// These mediators decorate the application lifecycle as needed.
	static class DemoMediator extends PumaContextMediator {
		// Override base class methods to customize the way that PUMA boots + runs, and
		// to receive notifications of progress during the boot / re-boot process.
		String TEST_REPO_SHEET_KEY = "0ArBjkBoH40tndDdsVEVHZXhVRHFETTB5MGhGcWFmeGc";
		int  DFLT_NAMESPACE_SHEET_NUM = 9;
		int   DFLT_DIRECTORY_SHEET_NUM = 8;
		
		public	PumaAppContext	myDemoPACtx;
		
		@Override public RepoSpec getMainConfigRepoSpec() {
			java.util.List<ClassLoader> fileResModelCLs = new java.util.ArrayList<ClassLoader>();
			return new OnlineSheetRepoSpec(TEST_REPO_SHEET_KEY, DFLT_NAMESPACE_SHEET_NUM, DFLT_DIRECTORY_SHEET_NUM,
							fileResModelCLs);
		}
		@Override public void notifyBeforeBootComplete(PumaAppContext ctx) throws Throwable {
			myDemoPACtx = ctx;
			// We could do some additional init here, if desired.
			// We are on the frameworkStartedCallback() thread.  
		}
	}	


}
