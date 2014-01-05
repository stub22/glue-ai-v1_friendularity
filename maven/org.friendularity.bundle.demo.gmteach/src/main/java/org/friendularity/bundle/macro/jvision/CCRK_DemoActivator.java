package org.friendularity.bundle.macro.jvision;

import java.util.List;

import org.appdapter.core.name.FreeIdent;
import org.appdapter.core.name.Ident;
import org.appdapter.osgi.core.BundleActivatorBase;
import org.cogchar.app.puma.boot.PumaAppContext;
import org.cogchar.app.puma.boot.PumaBooter;
import org.cogchar.app.puma.config.PumaContextMediator;
import org.cogchar.app.puma.registry.PumaGlobalPrebootInjector;
import org.cogchar.bind.rk.robot.motion.CogcharMotionSource;
import org.cogchar.bind.symja.MathGate;
import org.cogchar.bind.symja.MathSpaceFactory;
import org.cogchar.bundle.app.puma.PumaAppUtils;
import org.cogchar.scalatest.Whackamole;
import org.friendularity.gmteach.api.west.WorldEstimate;
import org.friendularity.gmteach.impl.visual.EstimateVisualizer;
import org.friendularity.gmteach.impl.visual.WorldEstimateRenderModule;
// import org.cogchar.test.symcalc.ScriptEngineExperiment;
import org.friendularity.gmteach.vworld.VisionDataFeed;
import org.osgi.framework.BundleContext;
import org.robokind.api.motion.Robot;
// import org.appdapter.gui.demo.DemoBrowser;
// import org.robokind.ui.swing.common.lifecycle.ServicesFrame;
import org.rwshop.swing.common.lifecycle.ServicesFrame;

/**
 * This class is a bundle activator demonstrating how to start the Cogchar PUMA system, in an OSGi environment. We call such a bundle a "top" bundle, which is essentially a launchable application. This bundle can be launched using a maven command line like:
 * 
 * mvn -Prun-on-felix antrun:run
 * 
 * ...which is equivalent to a project->run command in Netbeans or Eclipse.
 * 
 * When the bundle is loaded (in activator "start"), it activates Log4J logging using forceLog4jConfig(), which assumes a "log4j.properties" file-resource is available on the classpath.
 * 
 * In principle, this bundle should start any desired subset of Cogchar functionality, for *any* environment, including
 * <ul>
 * <li>Cogchar behavior</li>
 * <li>Robokind animation and speech</li>
 * <li>Optional Cogchar OpenGL rendering</li>
 * </ul>
 * 
 * The actual subset is determined through the mediation of a customizable PumaContextMediator object.
 * 
 * L1) This bundle does not try to start a lifter webapp, which is currently initialized orthogonally to the PUMA system. See the o.f.b.demo.liftoff project.
 * 
 * To exit, a user may X-closes our "main" simulator window, which calls stop on bundle 0.
 * 
 * @author Stu B. <www.texpedient.com>
 */
public class CCRK_DemoActivator extends BundleActivatorBase {

	private boolean myFlag_connectJVisionApp = false; // 2013-12-28 temp disabled JVision
	private boolean myFlag_visionMonitors = true;
	private boolean myFlag_connectMidiIn = true;
	private boolean myFlag_connectMidiOut = true;
	private boolean myFlag_connectMidiSwitcheroo = true;
	private boolean myFlag_connectSwingDebugGUI = false;
	private boolean myFlag_monitorLifecycles = true;

	private CCRK_DemoMidiCommandMapper myMidiMapper;

	@Override public void start(final BundleContext context) throws Exception {
		// take control for jVision
		//org.friendularity.bundle.jvision.JVisionBundleActivator.LAUNCH_MYSELF = false;
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
		CCRK_DemoMediator mediator = new CCRK_DemoMediator();
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
		startWhackamoleGuiWindow(context);

		//ScriptEngineExperiment.main(null);

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

	/** Not currently needed */
	protected void startWhackamoleGuiWindow(final BundleContext context) {
		Whackamole.launchWhackamoleGui(null);
	}

	@Override protected void handleFrameworkStartedEvent(BundleContext bundleCtx) {
		getLogger().info("Calling startPumaDemo()");
		startPumaDemo(bundleCtx);

		// Here we *could start some extra app-specific (e.g. Cogbot binding) goodies, and tell them to attach to 
		// PUMA  behavior system.  However, the Cogchar config system is intended to be sufficiently general to
		// handle most initialization cases without help from bundle activators.		
	}

	@Override public void stop(BundleContext context) throws Exception {
		super.stop(context);
	}

	private void startPumaDemo(BundleContext bundleCtx) {

		PumaBooter pumaBooter = new PumaBooter();
		PumaContextMediator mediator = PumaGlobalPrebootInjector.getTheInjector().getMediator();

		PumaBooter.BootResult bootResult = pumaBooter.bootUnderOSGi(bundleCtx, mediator);
		getLogger().info("Got PUMA BootResult: " + bootResult);
		// TODO:  Pay attention to {the set of relevant charIDs and component configs}, as we set up these
		// motionComputer + estimateVisualizer components.

		// Cheaters context is available locally only for our demo-specific debugging features.
		// [It is currently unused]
		// If another mediator took over instead, then we won't try to "cheat" to make those debugging features run.
		PumaAppContext localDemoCheatersContext = null;
		if (mediator instanceof CCRK_DemoMediator) {
			localDemoCheatersContext = ((CCRK_DemoMediator) mediator).myDemoPACtx;
		}

		// Hey, let's get some fused-sensor-data visualization going too, while we're at it!
		WorldEstimateRenderModule werm = new WorldEstimateRenderModule();

		// Enable/Disable this texture flow based on whether we are launching JVision or not.
		// Should be a dont-care whether this happens before/after   startVisionMonitors() below.
		// TODO:  Re-verify in detail.
		try {
			werm.setFlag_visionTextureRoutingEnabled(myFlag_visionMonitors && false);
		} catch (Throwable t) {
		}

		PumaAppUtils.attachVWorldRenderModule(bundleCtx, werm, null);
		EstimateVisualizer eViz = werm.setupVisualizer(null, null, null);
		// Needs to be done at least once for the selfEstim to exist.
		MathSpaceFactory msf = new MathSpaceFactory();
		// MathGate mg = msf.makeScriptedMathGate();
		MathGate mg = msf.makeUnscriptedMathGate();
		werm.setMathGate(mg);
		Ident worldEstimID = new FreeIdent(WorldEstimate.ESTIM_NS + "world_estim_31");
		WorldEstimate we = new WorldEstimate(worldEstimID);
		werm.setWorldEstimate(we);
		Robot.Id optRobotID_elseAllRobots = null;
		startMotionComputers(bundleCtx, optRobotID_elseAllRobots, we);

		if (myFlag_visionMonitors) {
			//	Startup the optional JVision connection
			try {
				startVisionMonitors();
			} catch (Throwable t) {
				t.printStackTrace();
			}
		}

		myMidiMapper = new CCRK_DemoMidiCommandMapper();

		if (myFlag_connectMidiIn) {
			myMidiMapper.startMidiRouters(werm);
		}
		if (myFlag_connectMidiOut) {
			// Does the VWorld block while this is running?  Why?
			myMidiMapper.startMidiOutputDemo();
		}
		if (myFlag_connectMidiSwitcheroo) {
			myMidiMapper.startMidiSwitcherooDemo();
		}
		if (myFlag_connectSwingDebugGUI) {
			setupDebuggingScaffold(mg, we);
		}
		if (localDemoCheatersContext != null) {
			getLogger().info("We have a cheater's Puma-App-Context, but we're not cheatin with it today - hooray!");
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
	 * For each joint robot, robokind.org blends all joint inputs received from RobotMoverFrameSources. Cogchar.org defines the CogcharMotionSource subclass. A cogcharMotionSource has an ordered list of CogcharMotionComputers.
	 * 
	 * @param bundleCtx
	 * @param optRobotID_elseAllRobots
	 */
	private void startMotionComputers(BundleContext bundleCtx, Robot.Id optRobotID_elseAllRobots, WorldEstimate we) {
		List<CogcharMotionSource> cogMotSrcList = CogcharMotionSource.findCogcharMotionSources(bundleCtx, optRobotID_elseAllRobots);

		for (CogcharMotionSource cms : cogMotSrcList) {
			Robot srcBot = cms.getRobot();
			Robot.Id srcBotID = srcBot.getRobotId();
			if ((optRobotID_elseAllRobots == null) || optRobotID_elseAllRobots.equals(srcBotID)) {
				getLogger().info("Found CogcharMotionSource for Robot-ID {} matching pattern {}", srcBotID, optRobotID_elseAllRobots);
				// Start a motion computer implemented locally in demo CCRK - tries to swing Sinbad around his spine
				CCRK_DemoMotionComputer dmc = new CCRK_DemoMotionComputer();
				dmc.setWorldEstimate(we);
				cms.addJointComputer(dmc);
				// append a trivial Sinbad-waist-sinusoid-demo implemented in CC-Puma.  Because it acts last, it has last
				// word, but should not unnecessarily override joint-pos from "earlier" phases=computers.
				// PumaAppUtils.startSillyMotionComputersDemoForVWorldOnly(bundleCtx, srcBotID); 		
			} else {
				getLogger().debug("Skipping Robot-ID {} because it doesn't match pattern {}", srcBotID, optRobotID_elseAllRobots);
			}
		}
	}

	private void startVisionMonitors() {
		VisionDataFeed vdf = new VisionDataFeed();
		boolean svcsOK = vdf.connectServices();
		getLogger().info("vdf.connectServices returned {}", svcsOK);
		if (svcsOK) {
			vdf.registerDummyListeners();
			vdf.startServices();
		}
	}

}
