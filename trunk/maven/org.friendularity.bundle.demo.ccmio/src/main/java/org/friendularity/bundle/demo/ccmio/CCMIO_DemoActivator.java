package org.friendularity.bundle.demo.ccmio;

import akka.actor.ActorSystem;
import org.appdapter.fancy.rclient.EnhancedLocalRepoClient;
import org.appdapter.osgi.core.BundleActivatorBase;
import org.cogchar.bind.symja.MathGate;
import org.cogchar.blob.entry.EntryHost;
import org.friendularity.api.west.WorldEstimate;
import org.friendularity.navui.NavUiAppImpl;
import org.friendularity.navui.NavUiAppSvc;
import org.friendularity.old.ccmio.OldLaunchHelper;
import org.friendularity.vsim.vworld.UnusedNetworkVisionDataFeed;
import org.osgi.framework.BundleContext;
import org.rwshop.swing.common.lifecycle.ServicesFrame;

import org.friendularity.appro.TestRaizLoad;

import com.hp.hpl.jena.rdf.model.Model;

// import org.cogchar.test.symcalc.ScriptEngineExperiment;
// import org.mechio.ui.swing.common.lifecycle.ServicesFrame;
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
 * In principle, this bundle can start any desired subset of Cogchar functionality,
 * for any combination of chars and deployment environment.  Avail features include:
 *		<ul><li>Cogchar behavior
 *		</li><li>MechIO animation and speech
 *		</li><li>Optional Cogchar OpenGL rendering</li></ul>

*  The actual subset is determined through
 *  1) RDF Config
 *  2) Optional mediation of a customizable PumaContextMediator object.

* L1) This bundle does not try to start a liftweb webapp, which is currently initialized
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

	// These flags control feature activation.
	// TODO: Replace with flag values sourced from profile.
	public static	boolean		myFlag_connectJVision = true;  	      // Read JNI (JNA?) vision stream
	private	boolean		myFlag_connectObsoleteNetworkVision = false;  // Read QPid vision streams

	private	boolean		myFlag_connectSwingDebugGUI = false;  // Swing debug code disabled, anyway
	private boolean		myFlag_monitorLifecycles = true;  // LifeMon window is launched by .start()

	private boolean 	myFlag_useOldLaunchStyle2014 = false;
	// attach... flag now used only during old launch style 2014
	public static	boolean		myFlag_attachVizappTChunkRepo = true; // false => uses old vanilla mediator backup

	private Class 		myProfileMarkerClz = TestRaizLoad.class;
	private Class 		myLegConfMarkerClz = TestRaizLoad.class;

	private OldLaunchHelper myOldLaunchHelper;

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

		startAkkaOSGi(context);

		if (myFlag_useOldLaunchStyle2014) {
			myOldLaunchHelper = new OldLaunchHelper();
			// Preliminary step allows fallback to old-old 2012 way (doubly outdated now, really).
			myOldLaunchHelper.registerOldMediatorStuff_duringStart();
		}

		// Schedule our callback to the handle method below.
		scheduleFrameworkStartEventHandler(context);
		if (myFlag_monitorLifecycles) {
			startLifecycleMonitorGuiWindow(context);
		}
		getLogger().info("Setting JVision.LAUNCH_MYSELF to {}", myFlag_connectJVision);
		org.friendularity.bundle.jvision.JVisionBundleActivator.setLaunchFlag(myFlag_connectJVision);
	}
	@Override public void stop(BundleContext context) throws Exception {
		super.stop(context);
	}

	// This callback after OSGi bundles loaded handles the main init process
	@Override protected void handleFrameworkStartedEvent(BundleContext bundleCtx) {
		launchCcmioDemo(bundleCtx);
	}

	private void launchCcmioDemo(BundleContext bundleCtx) {
		getLogger().info("============ launchCcmioDemo BEGIN  ==========");
		EntryHost	profileEHost = TestRaizLoad.makeBundleEntryHost(myProfileMarkerClz);
		Model mergedProfileJM = loadMergedProfileGraph(profileEHost);
		if (mergedProfileJM == null) {
			throw new RuntimeException("launchCcmioDemo cannot read profile from classpath containing " + myProfileMarkerClz);
		}
		EntryHost	legConfEHost = TestRaizLoad.makeBundleEntryHost(myLegConfMarkerClz);
		if (myFlag_useOldLaunchStyle2014) {
			if (myFlag_attachVizappTChunkRepo) {
				// OLD launch mechanism, which we keep working for comparative testing
				getLogger().info("============= 2014-style launch is calling attachVizTChunkLegConfRepo() ======");
				attachVizTChunkLegConfRepo(bundleCtx, mergedProfileJM, legConfEHost);
			} // else we would be seeing fallback injected mediator in control

			myOldLaunchHelper.startOldPumaThenVWorld(bundleCtx);
		} else {
			// 2016 way:
			ActorSystem akkaSys = myCPumpHelper.dangerActorSysExposed();  // Should be avail because startAkkaOSGi was called during .start().
			launchVWorldWithSinbad_2016(bundleCtx, akkaSys, mergedProfileJM, legConfEHost);
		}
		getLogger().info("============ Calling launchCPumpService() ==========");
		launchCPumpService(bundleCtx);
		getLogger().info("============ Calling launchOtherStuffLate() ==========");
		launchOtherStuffLate();
		getLogger().info("============ launchCcmioDemo END  ==========");
		// Here we *could start some extra app-specific (e.g. Cogbot binding) goodies, and tell them to attach to
		// PUMA  behavior system.  However, the Cogchar config system is intended to be sufficiently general to
		// handle most initialization cases without help from bundle activators.
	}
	public void launchVWorldWithSinbad_2016(BundleContext bundleCtx, ActorSystem akkaSys,
											Model mergedProfileJM,	EntryHost legConfEHost) {
		// Launches OpenGL world and actors for talking to it.
		// Can be tested separately using the TestNavUI.main() launcher.

		NavUiAppSvc appSvc = startVWorldNavUI_2016(bundleCtx, akkaSys);

		// Now the VWorld is up and accepting messages, but there is no char in it yet.
		// Temporary revised compromise here - keep using the old ("legacy") data known to work until we
		// can prove that newer data is workin.
		// So we now load an old config chunk of ~30 turtle files, most of which
		// are unused.  We really just want the bone mappngs + body mesh-names,
		// which occupy just a few of these loaded graphs.

		getLogger().info("============= 2016 semi-legacy launcher calling makeLegacyELRC() ======");
		EnhancedLocalRepoClient elrc = makeLegacyELRC(mergedProfileJM, legConfEHost);

		getLogger().info("============= 2016 semi-legacy launcher calling startSemiLegacyBodyConn_OSGi_Sinbad() ======");
		// This method instantiates necessary config objects and outer callback ("bodyNoticer"),
		// and then enqueues an async request for the char-admin actor.

		appSvc.startSemiLegacyBodyConn_OSGi_Sinbad(bundleCtx, akkaSys, elrc);
		getLogger().info("============= 2016 semi-legacy VWorld + Body launcher is done sending messages  ======");

	}
	private NavUiAppImpl startVWorldNavUI_2016(BundleContext bundleCtx, ActorSystem akkaSys) {
		NavUiAppImpl nuiApp = new NavUiAppImpl(akkaSys);
		getLogger().info("^^^^^^^^^^^^^^^^^^^^^^^^  TestNavUI.main() created nuiApp={}", nuiApp);
		nuiApp.sendSetupMsgs_Async();
		return nuiApp;
	}

	// profileDataMarkerClz should have same classpath (i.e. same OSGi bundle) as the profile data.
	private Model loadMergedProfileGraph(EntryHost	 profileEHost) {
		Model mergedProfileGraph = TestRaizLoad.getMergedProfileGraph_RegularDesktop(profileEHost);
		return mergedProfileGraph;
	}
	// When active this VizTChunk removes the need for DemoMediator setup at bottom of this file.
	private void attachVizTChunkLegConfRepo(final BundleContext bunCtx, Model mergedProfileGraph, EntryHost legConfEHost) {
		// Same eHost is used here for profile and config data, but separate eHosts is also OK.
		// Easiest way to identify an bundleEHost is to specify a class from same bundle.
		EnhancedLocalRepoClient legacyConfERC = makeLegacyELRC(mergedProfileGraph, legConfEHost);

		TestRaizLoad.registerAvatarConfigRepoClient(bunCtx, legacyConfERC);
	}
	private EnhancedLocalRepoClient makeLegacyELRC(Model mergedProfileGraph, EntryHost legConfEHost) {
		String vzBrkRcpUriTxt = TestRaizLoad.vzpLegCnfBrkrRcpUriTxt();
		EnhancedLocalRepoClient legacyConfERC = TestRaizLoad.makeAvatarLegacyConfigRepo(mergedProfileGraph,
					vzBrkRcpUriTxt, legConfEHost);
		getLogger().info("legConfEnhRepoCli={}", legacyConfERC);
		return legacyConfERC;
	}

	private CCMIO_CPumpHelper myCPumpHelper = new CCMIO_CPumpHelper();

	private void startAkkaOSGi(BundleContext bundleCtx) {
		myCPumpHelper.startAkkaOSGi(bundleCtx);
		// Now the actorSystem should be available via myCPumpHelper.dangerExposedActorSys
	}

	private void launchCPumpService(BundleContext bundleCtx) {

		myCPumpHelper.launchCPump(bundleCtx);
	}


	private void launchOtherStuffLate()  {
	
		if (myFlag_connectObsoleteNetworkVision) {
			//	Startup alternate QPid network vision connection (separate from JVision)
			startObsoleteNetworkVisionMonitors();
		}
		if (myFlag_connectSwingDebugGUI) {
			throw new RuntimeException("SwingDebugGUI is disabled, so flag should be false.");
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
	 *  For each joint robot, mechio.org blends all joint inputs received from RobotMoverFrameSources.
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
	 // New bugs in ScreenBoxImpl are preventing this window from launching.
	 // startWhackamoleGuiWindow(context)
	 // ScriptEngineExperiment.main(null);
	 */

}
