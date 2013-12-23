package org.friendularity.bundle.macro.jvision;

import static org.friendularity.bundle.macro.tools.R50ConfigUtils.JOINT_GROUP_XML_CONFIG_PATH;
import static org.friendularity.bundle.macro.tools.R50ConfigUtils.ROBOT_XML_CONFIG_PATH;
import static org.friendularity.bundle.macro.tools.R50ConfigUtils.VISEME_JSON_CONFIG_PATH;

import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Level;

import javax.swing.JLabel;
import javax.swing.UIManager;

import org.apache.log4j.PropertyConfigurator;
import org.appdapter.core.matdat.EnhancedRepoClient;
import org.appdapter.core.matdat.RepoSpec;
import org.appdapter.core.name.FreeIdent;
import org.appdapter.core.name.Ident;
import org.appdapter.core.store.Repo;
import org.appdapter.gui.demo.DemoBrowser;
import org.appdapter.help.repo.RepoClient;
import org.cogchar.api.thing.WantsThingAction;
import org.cogchar.app.puma.boot.PumaAppContext;
import org.cogchar.app.puma.boot.PumaBooter;
import org.cogchar.app.puma.config.PumaContextMediator;
import org.cogchar.app.puma.registry.PumaGlobalPrebootInjector;
import org.cogchar.app.puma.web.LifterLifecycle;
import org.cogchar.bind.midi.FunMidiEventRouter;
import org.cogchar.bind.midi.out.CogcharMidiOutputTestMain;
import org.cogchar.bind.rk.behavior.SceneLifecycleDemo;
import org.cogchar.bind.rk.robot.client.AnimOutTrigChan;
import org.cogchar.bind.rk.robot.client.RobotAnimContext;
import org.cogchar.bind.rk.robot.motion.CogcharMotionSource;
import org.cogchar.bind.symja.MathGate;
import org.cogchar.bind.symja.MathGateUnscripted;
import org.cogchar.bind.symja.MathSpaceFactory;
import org.cogchar.blob.emit.BehaviorConfigEmitter;
import org.cogchar.bundle.app.puma.GruesomeTAProcessingFuncs;
import org.cogchar.bundle.app.puma.PumaAppUtils;
import org.cogchar.impl.channel.AnimFileSpecReader;
import org.cogchar.impl.scene.read.BehavMasterConfigTest;
import org.cogchar.joswrap.RepoUpdateCallbackAdapter;
import org.cogchar.render.trial.TrialBalloon;
import org.cogchar.svc.behav.control.BehaviorControlServiceManager;
import org.friendularity.bundle.macro.behavior.BehaviorMasterConfig;
import org.friendularity.bundle.macro.behavior.BehaviorMasterLifecycle;
import org.friendularity.bundle.macro.behavior.HeadlessBehaviorLifecycle;
import org.friendularity.bundle.macro.behavior.TriggerFrame;
import org.friendularity.bundle.macro.common.CommonActivator;
import org.friendularity.bundle.macro.common.CommonMediator;
import org.friendularity.bundle.macro.tools.R50ConfigUtils;
import org.friendularity.gmteach.api.west.WorldEstimate;
import org.friendularity.gmteach.impl.visual.EstimateVisualizer;
import org.friendularity.gmteach.impl.visual.WorldEstimateRenderModule;
import org.friendularity.gmteach.vworld.VisionDataFeed;
import org.jflux.api.core.config.Configuration;
import org.jflux.api.core.config.DefaultConfiguration;
import org.jflux.impl.registry.OSGiRegistry;
import org.jflux.impl.services.rk.lifecycle.AbstractLifecycleProvider;
import org.jflux.impl.services.rk.lifecycle.ServiceLifecycleProvider;
import org.jflux.impl.services.rk.lifecycle.utils.DescriptorListBuilder;
import org.jflux.impl.services.rk.osgi.lifecycle.OSGiComponent;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;
import org.osgi.framework.BundleEvent;
import org.osgi.framework.BundleListener;
import org.osgi.framework.FrameworkEvent;
import org.osgi.framework.ServiceEvent;
import org.osgi.framework.ServiceListener;
import org.robokind.api.animation.player.AnimationPlayer;
import org.robokind.api.motion.Robot;
import org.rwshop.swing.common.lifecycle.ServicesFrame;
import org.rwshop.swing.messaging.monitor.AvroTableDemoFrame;
import org.slf4j.Logger;

// import org.appdapter.gui.demo.DemoBrowser;

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
 * The actual subset is determined through the intervation of a customizable PumaContextMediator object.
 * 
 * L1) This bundle does not try to start a lifter webapp, which is currently initialized orthogonally to the PUMA system. See the o.f.b.demo.liftoff project.
 * 
 * To exit, a user may X-closes our "main" simulator window, which calls stop on bundle 0.
 * 
 * @author Stu B. <www.texpedient.com>
 */
public class LikeSuperActivator extends CommonActivator implements BundleListener, ServiceListener {

	protected boolean isHeadless() {
		return HEADLESS;
	}

	@Override
	public void start(BundleContext bundleCtx) throws Exception {
		m_context = bundleCtx;
		HEADLESS = false;
		MACRO_LAUNCHER = false;
		// TODO Auto-generated method stub
		super.start(bundleCtx);
		bundleCtx.removeBundleListener(this);
		bundleCtx.addBundleListener(this);
		bundleCtx.removeServiceListener(this);
		bundleCtx.addServiceListener(this);
		bundleCtx.removeFrameworkListener(this);
		bundleCtx.addFrameworkListener(this);
		super.bundleBootPhase = BootPhaseConst.UNSTARTED;
		if (!MACRO_LAUNCHER) {
			getLogger().error("org.friendularity.bundle.macrolauncher was not preceeded by org.friendularity.bundle.macro.common");
		}
		// take ownership as the primary bundle
		macroStartupSettings.makeStartupBundle(this);
		setupDefaultConfigs(bundleCtx);
		macroStartupSettings.raiseToPhase(MacroStartupSettings.COMPLETED_REGISTERSERVICES);
		macroStartupSettings.printMacroStateInfo(System.err, m_context);
	}

	/**
	 * Receives notification of a general {@code FrameworkEvent} object.
	 * 
	 * @param event
	 *            The {@code FrameworkEvent} object.
	 */
        @Override public void frameworkEvent(FrameworkEvent event) {
		int eventType = event.getType();
		Throwable throwable = event.getThrowable();
		Bundle bundle = event.getBundle();
		if (throwable != null)
			throwable.printStackTrace();
		if (eventType == FrameworkEvent.STARTED) {
			getLogger().info("In OSGi framework-started callback, initialization of GMTEACH");
			try {
				launchApplication(bundle.getBundleContext());
			} catch (Exception e) {
				e.printStackTrace();
			}
			return;
		}
		System.err.println("framework event " + event);
	}

	@Override public boolean isLauncherBundle() {
		return true;
	}

	@Override
	public void serviceChanged(ServiceEvent event) {
		System.err.println("serviceChanged event " + event);

	}

	@Override
	public void bundleChanged(BundleEvent event) {
		System.err.println("bundle event " + event);
	}

	void setDefaultSettings(boolean startAll) {
		putSetting("connectJVision", startAll);
		putSetting("connectMidiIn", startAll);
		putSetting("connectMidiOut", startAll);
		putSetting("whackamole", true);
		putSetting("monitorLifecycles", startAll);
		putSetting("VirtualWorld", startAll);
		putSetting("Behavior", startAll);
		putSetting("AvroMessages", startAll);
		putSetting("BootPuma", startAll);
		putSetting("DeicticVisualizer", startAll);
	}

	private void putSetting(String key, boolean value) {
		macroStartupSettings.putSetting(key, value);

	}

	private void setupDefaultConfigs(final BundleContext context) {
		setDefaultSettings(true);
		// Have to set this bool early in start() here
		org.friendularity.bundle.jvision.JVisionBundleActivator.LAUNCH_MYSELF = false;// flagTrue("connectJVision");
		//putSetting("VirtualWorld", false);
		putSetting("BootPuma", true);
		putSetting("worldEstimate", true);
		putSetting("scenelifecycledemotest", true);

	}

	public void startQPIDMonitor(final BundleContext context) {
		getLogger().info("AQServiceSwingUI Activation Begin.");
		java.awt.EventQueue.invokeLater(new Runnable() {

			public void run() {
				AvroTableDemoFrame frame = new AvroTableDemoFrame();
				frame.start(context);
				frame.setVisible(true);
			}
		});
		getLogger().info("AQServiceSwingUI Activation Complete.");
	}

	public void launchApplication(BundleContext context) throws Exception {
		if (this.bundleBootPhase < BootPhaseConst.LAUNCHING) {
			this.bundleBootPhase = BootPhaseConst.LAUNCHING;
			macroStartupSettings.makeStartupBundle(this);
			if (!isLauncherBundle()) {
				showDemoAppStarter();
			}
			launchDemoApp();
			this.bundleBootPhase = BootPhaseConst.LAUNCHING_COMPLETE;
		}
	}

	public void launchDemoApp() {
		new Thread(new Runnable() {
			@Override
			public void run() {
				runDemoApp();
			}
		}, "launchDemoApp " + this).start();
	}

	public void runDemoApp() {
		try {
			getLogger().warn("runDemoApp Activation in 10 seconds.");
			Thread.sleep(10000);
		} catch (InterruptedException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		// How to get the cmd line args if we need them
		getLogger().warn("gmteach.glulx Activation in 0 seconds.");
		String args = macroStartupSettings.getProperty(LikeSuperActivator.this.m_context.getBundle(), "gmteach.glulx", "inform7.story", "glulx.args", "application.args", "launcher.arguments").trim();
		macroStartupSettings.putSetting("inform7.story", args);
		// raise to POST_CONFIG
		macroStartupSettings.raiseToPhase(MacroStartupSettings.POST_CONFIG);
		// show POST_CONFIG
		macroStartupSettings.printMacroStateInfo(System.err, m_context);
		getLogger().warn("launchPhases in 0 seconds.");
		macroStartupSettings.launchPhases();
		getLogger().warn("launchPhases COMPLETE.");
	}

	public void showDemoAppStarter() {
		DemoBrowser.appName = "MacroLauncher";
		DemoBrowser.addRepoLoaderMenu();
		DemoBrowser.showObject(macroStartupSettings);
		org.appdapter.gui.browse.Utility.setSingletonValue(BundleContext.class, m_context);
		DemoBrowser.show();
	}

	protected CommonMediator getMediator() {
		return new CommonMediator(m_context);
	}

	static class AnimChanLifecycle extends AbstractLifecycleProvider<WantsThingAction, AnimOutTrigChan> {
		private List<ClassLoader> myResourceClassLoaders;

		public AnimChanLifecycle(String robotId, List<ClassLoader> resourceClassLoaders) {
			super(new DescriptorListBuilder().dependency("animPlayer", AnimationPlayer.class).with(AnimationPlayer.PROP_PLAYER_ID, robotId).dependency("repoClient", RepoClient.class).getDescriptors());
			if (myRegistrationProperties == null) {
				myRegistrationProperties = new Properties();
			}
			myRegistrationProperties.put("thingActionChannelAgentId", robotId);
			myResourceClassLoaders = resourceClassLoaders;
		}

		@Override
		protected AnimOutTrigChan create(Map<String, Object> dependencies) {
			RepoClient rc = (RepoClient) dependencies.get("repoClient");
			BehaviorConfigEmitter bce = new BehaviorConfigEmitter(rc, rc.makeIdentForQName(AnimFileSpecReader.animGraphQN()));
			AnimationPlayer player = (AnimationPlayer) dependencies.get("animPlayer");
			RobotAnimContext rac = new RobotAnimContext(new FreeIdent("http://hrkind.com/robot#zenoR50"), bce);
			rac.setResourceClassLoaders(myResourceClassLoaders);
			rac.initConnForAnimPlayer(player);
			return rac.getTriggeringChannel();
		}

		@Override
		protected Class<WantsThingAction> getServiceClass() {
			return WantsThingAction.class;
		}

		@Override
		protected void handleChange(String name, Object dependency, Map<String, Object> availableDependencies) {
		}

	}

	/**
	 * Copied from Activator for Friendularity.Liftoff.
	 * 
	 * @param context
	 */
	public void initLifterWebappLifecycle(BundleContext context) {
		ensurePuma();
		// Tell the lifter lifecycle to start, once its dependencies are satisfied
		LifterLifecycle lifecycle = new LifterLifecycle();
		OSGiComponent lifterComp = new OSGiComponent(context, lifecycle);
		lifterComp.start();
	}

	WorldEstimateRenderModule werm;
	EstimateVisualizer<?> eViz;
	protected MathGateUnscripted mg;
	private WorldEstimate we;

	public void registerServices(BundleContext context0) {

		addMacroPreService("useBehaviorRepoConfig", new Runnable() {
			@Override
			public void run() {
				installSharedRepo(m_context);
			}
		});

		addMacroPreService("configPuma", new Runnable() {
			@Override
			public void run() {
				configPuma();
			}
		});

		addMacroPreService("bootPuma", new Runnable() {
			@Override
			public void run() {
				ensurePuma();
			}
		});

		addMacroService("trialBaloon", new Runnable() {
			@Override
			public void run() {
				TrialBalloon.main(new String[0]);
			}
		});

		addMacroService("midiOutputTest", new Runnable() {
			@Override
			public void run() {
				CogcharMidiOutputTestMain.main(new String[0]);
			}
		});

		addMacroService("connectJVision", new Runnable() {
			@Override
			public void run() {
				//	Startup the optional JVision connection
				startVisionMonitors();
			}
		});

		addMacroService("setSystemLookAndFeel", new Runnable() {
			@Override
			public void run() {
				try {
					if (isHeadless())
						return;
					UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
				} catch (Exception ex) {
					java.util.logging.Logger.getLogger(AvroTableDemoFrame.class.getName()).log(java.util.logging.Level.SEVERE, null, ex);
				}
			}
		});
		addMacroService("monitorLifecycles", new Runnable() {
			@Override
			public void run() {
				startServicePanel(m_context);
			}
		});

		addMacroService("worldEstimate", new Runnable() {
			@Override
			public void run() {
				ensurePuma();

				// Hey, let's get some fused-sensor-data visualization going too, while we're at it!
				werm = new WorldEstimateRenderModule();
				// Enable/Disable this texture flow based on whether we are launching JVision or not.
				// Should be a dont-care whether this happens before/after   startVisionMonitors() below.
				// TODO:  Re-verify in detail.
				werm.setFlag_visionTextureRoutingEnabled(macroStartupSettings.flagTrue("connectJVision"));
			}
		});

		addMacroService("attachVWorldRenderModule", new Runnable() {
			@Override
			public void run() {
				PumaAppUtils.attachVWorldRenderModule(m_context, werm, null);
			}
		});
		addMacroService("setupVisualizer", new Runnable() {
			@Override
			public void run() {
				eViz = werm.setupVisualizer(null, null, null);
			}
		});

		addMacroService("makeUnscriptedMathGate", new Runnable() {
			@Override
			public void run() {

				// Needs to be done at least once for the selfEstim to exist.
				MathSpaceFactory msf = new MathSpaceFactory();
				// MathGate mg = msf.makeScriptedMathGate();
				LikeSuperActivator.this.mg = msf.makeUnscriptedMathGate();
				werm.setMathGate(mg);

			}
		});
		addMacroService("world_estim_31", new Runnable() {
			@Override
			public void run() {
				Ident worldEstimID = new FreeIdent(WorldEstimate.ESTIM_NS + "world_estim_31");
				LikeSuperActivator.this.we = new WorldEstimate(worldEstimID);
				werm.setWorldEstimate(we);
			}
		});

		addMacroService("startMotionComputers", new Runnable() {
			@Override
			public void run() {

				Robot.Id optRobotID_elseAllRobots = null;
				startMotionComputers(m_context, optRobotID_elseAllRobots, we);
			}
		});

		addMacroService("connectMidiIn", new Runnable() {
			@Override
			public void run() {
				startMidiRouters(werm);
			}
		});

		addMacroService("whackamole", new Runnable() {
			@Override
			public void run() {
				setupDebuggingScaffold(mg, we);
			}
		});

		addMacroService("BehaviorControlServiceManager", new Runnable() {
			@Override
			public void run() {
				BehaviorControlServiceManager.init(new OSGiRegistry(m_context));
			}
		});

		addMacroService("SpeechRecognitionServiceManager", new Runnable() {
			@Override
			public void run() {
				// Still TODO
				//	SpeechRecognitionServiceManager
				//		.init(new OSGiRegistry(m_context));
			}
		});

		addMacroService("R50ConfigUtils.startR50", new Runnable() {
			@Override
			public void run() {
				startR50(m_context);
			}
		});

		addMacroService("initLifterWebappLifecycle", new Runnable() {
			@Override
			public void run() {
				initLifterWebappLifecycle(m_context);
			}
		});
		addMacroService("ScriptEngineExperiment", new Runnable() {
			@Override
			public void run() {
				try {
					Class.forName("org.cogchar.test.symcalc.ScriptEngineExperiment").getMethod("main", String[].class).invoke(null, null);
				} catch (Exception e) {
					e.printStackTrace();
				}
				//ScriptEngineExperiment.main(null);
			}
		});

		addMacroService("SceneLifecycleDemoTest", new Runnable() {
			@Override
			public void run() {
				SceneLifecycleDemo.test(m_context);
				/*  [java] java.lang.RuntimeException: Uri does not contain text after hash '#' [fakeURI]
					 [java] 	at org.appdapter.core.name.FreeIdent.<init>(FreeIdent.java:40)
					 [java] 	at org.cogchar.bind.rk.behavior.ChannelBindingConfig.initExplicitly(ChannelBindingConfig.java:41)
					 [java] 	at org.cogchar.bind.rk.behavior.SceneLifecycleDemo.test(SceneLifecycleDemo.java:66)
					 [java] 	at org.friendularity.bundle.demo.ccrk.CCRK_DemoActivator.startLifecycleMonitorGuiWindow(CCRK_DemoActivator.java:73)
					 [java] 	at org.friendularity.bundle.demo.ccrk.CCRK_DemoActivator.start(CCRK_DemoActivator.java:63)
				**/
			}
		});

		addMacroService("AnimChanLifecycle", new Runnable() {
			@Override
			public void run() {
				new OSGiComponent(m_context, new AnimChanLifecycle("myRobot", getMediator().getFileResClassLoaders())).start();
			}
		});

		addMacroService("startSillyMotionComputersDemoForVWorldOnly", new Runnable() {
			@Override
			public void run() {
				Robot.Id optRobotID_elseAllRobots = null;
				PumaAppUtils.startSillyMotionComputersDemoForVWorldOnly(m_context, optRobotID_elseAllRobots);
			}
		});

		addMacroService("checkAnimFiles", new Runnable() {
			@Override
			public void run() {
				PumaAppUtils.checkAnimationFiles(null);
			}
		});

		/*
		addMacroService("deicticVisualizer", new Runnable() {
			public void run() {
				DeicticVisualizer deictViz = new DeicticVisualizer();
				deictViz.forceHeadCameraOntoSinbad();
			}
		});*/

		addMacroService("startQPIDMonitor", new Runnable() {
			@Override
			public void run() {
				startQPIDMonitor(m_context);
			}
		});

		addMacroService("setupRepoUpdateCallback", new Runnable() {

			@Override
			public void run() {
				setupRepoUpdateCallback();
			}
		});

		//************************************************************************************************
		// With this call enabled, we get a Permgen during Oglweb.r25 init on JDK6-Win7x64 with -XX:MaxPermSize=512M.
		// With it disabled, we are hunky-dory?
		addMacroService("launchBehaviorLifecycles", new Runnable() {
			@Override
			public void run() {
				launchBehaviorLifecycles(m_context);
			}
		});

		//************************************************************************************************

		addMacroService("launchTriggerPanel", new Runnable() {
			@Override
			public void run() {
				startTriggerPanelSeparately(m_context);
			}
		});

		addMacroService("addConfigDemo", new Runnable() {
			@Override
			public void run() {

				addConfigs(m_context);
			}
		});

		// perhaps use a system property instead
		// setLookAndFeel();

	}

	private void addConfigs(BundleContext context) {

		DefaultConfiguration<String> inConfig = new DefaultConfiguration<String>();
		inConfig.addProperty(Integer.class, "intProp", 4);
		inConfig.addProperty(Double.class, "dblProp", 4.0);
		inConfig.addProperty(Byte.class, "byteProp", (byte) 4);
		inConfig.addProperty(String.class, "strProp2", "four");
		inConfig.addProperty(JLabel.class, "otherProp", new JLabel("four"));
		inConfig.addProperty(DefaultConfiguration.class, "confProp", inConfig);

		DefaultConfiguration<String> config = new DefaultConfiguration<String>();
		config.addProperty(Integer.class, "intProp", 5);
		config.addProperty(Double.class, "dblProp", 5.0);
		config.addProperty(Byte.class, "byteProp", (byte) 5);
		config.addProperty(String.class, "strProp", "five");
		config.addProperty(DefaultConfiguration.class, "confProp", inConfig);
		config.addProperty(JLabel.class, "otherProp", new JLabel("five"));

		context.registerService(Configuration.class.getName(), config, null);
		context.registerService(Configuration.class.getName(), inConfig, null);
	}

	private void launchBehaviorLifecycles(BundleContext context) {

		final ServiceLifecycleProvider lifecycle;

		if (!isHeadless()) {
			lifecycle = new BehaviorMasterLifecycle(context);
			/// BehaviorMasterLifecycle code does this on it's own
			macroStartupSettings.putSetting("launchTriggerPanel", false);
		} else {
			lifecycle = new HeadlessBehaviorLifecycle(context);
		}

		new OSGiComponent(context, lifecycle).start();
	}

	private void installSharedRepo(BundleContext context) {

		BehaviorMasterConfig n = new BehaviorMasterConfig();
		boolean useURLOrFile = false;

		String envVar = macroStartupSettings.getSetting(REPO_URL_VAR);
		if (envVar != null) {
			envVar = envVar.trim();
			if (!envVar.isEmpty()) {
				n.loadDataFromConfig();
				useURLOrFile = true;
				n.setWorkbookPath(envVar);
			}
		}

		RepoSpec repoSpec;

		if (useURLOrFile) {
			// Setup to collect an OfflineSheet repo specified by POM configuration
			//  FYI: Offline = local file or URI
			repoSpec = n.makeBMC_OfflineXlsSheetRepoSpec(context);
		} else {
			// Setup to connect to a GoogSheet repo specified by POM configuration
			repoSpec = n.makeBMC_OnlineSheetRepoSpec(context);
		}

		Repo.WithDirectory bmcMemoryRepoHandle = repoSpec.makeRepo();
		EnhancedRepoClient enhancedRepoSpec = new EnhancedRepoClient(repoSpec, bmcMemoryRepoHandle, (String) BehavMasterConfigTest.TGT_GRAPH_SPARQL_VAR(), BehavMasterConfigTest.QUERY_SOURCE_GRAPH_QN());

		context.registerService(RepoClient.class.getName(), enhancedRepoSpec, null);
	}

	private void setLookAndFeel() {
		javax.swing.SwingUtilities.invokeLater(new Runnable() {
			@Override
			public void run() {
				try {
					UIManager.setLookAndFeel(UIManager.getCrossPlatformLookAndFeelClassName());
				} catch (Exception ex) {
				}
			}
		});
	}

	// made public for outside control
	public void startServicePanel(final BundleContext context) {
		java.awt.EventQueue.invokeLater(new Runnable() {
			@Override
			public void run() {
				if (isHeadless())
					return;
				ServicesFrame.create(context);
			}
		});
	}

	// made public for outside control
	public void startTriggerPanelSeparately(final BundleContext context) {
		java.awt.EventQueue.invokeLater(new Runnable() {
			@Override
			public void run() {
				if (isHeadless())
					return;
				TriggerFrame tf = new TriggerFrame();
				String adminPanelId = "adminID";
				String behaviorPanelId = "behaviorID";
				tf.init(context, behaviorPanelId, adminPanelId);
				tf.setVisible(true);
			}
		});
	}

	private void setupDebuggingScaffold(MathGate mg, WorldEstimate we) {
		DemoBrowser.showObject("werm-MG", mg, false, false); // true, true);
		DemoBrowser.showObject("amazingly accurate estimate", we, false, false);
		PumaAppUtils.GreedyHandleSet greedyHandles = PumaAppUtils.obtainGreedyHandleSet();
		DemoBrowser.showObject("our-greedy-handles", greedyHandles, false, false);
		DemoBrowser.showObject("our-repo-client", greedyHandles.rc, false, false);
	}

	public void setupRepoUpdateCallback() {
		RepoUpdateCallbackAdapter.registerCallback(new RepoUpdateCallbackAdapter.Callback() {
			public void repoUpdateCompleted() {
				getLogger().info("c.h.b.oglweb.R50 activator got SPARQL-UPDATE callback");
				GruesomeTAProcessingFuncs.processPendingThingActions();
			}
		});
	}

	/*
		private void startDeicticVisualizer(BaseVisualizer bViz) {

			DeicticVisualizer deictViz = new DeicticVisualizer(bViz);
			//deictViz.ensureSetUp();// connectToTrialContent(bViz.getTrialContent());
			deictViz.forceHeadCameraOntoSinbad();
			deictViz.putVizPyramidOnDefaultCam();
			deictViz.setupNiftyPointingRays();
			bViz.myDVHackForUpdate = deictViz;
		}
	*/
	private void startMidiRouters(WorldEstimateRenderModule werm) {
		FunMidiEventRouter fmer = new FunMidiEventRouter();
		MidiCommandMapper mcm = new MidiCommandMapper();
		mcm.myWERM = werm;
		fmer.registerListener(mcm);
		//FunMidiEventRouter.FunListener fl = new FunMidiEventRouter.FunListener();
		//fmer.registerListener(fl);
		fmer.startPumpingMidiEvents();
	}

	/**
	 * For each joint robot, robokind.org blends all joint inputs received from RobotMoverFrameSources. Cogchar.org defines the CogcharMotionSource subclass. A cogcharMotionSource has an ordered list of CogcharMotionComputers.
	 * 
	 * @param context
	 * @param optRobotID_elseAllRobots
	 */
	private void startMotionComputers(BundleContext context, Robot.Id optRobotID_elseAllRobots, WorldEstimate we) {
		List<CogcharMotionSource> cogMotSrcList = CogcharMotionSource.findCogcharMotionSources(context, optRobotID_elseAllRobots);

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
				// PumaAppUtils.startSillyMotionComputersDemoForVWorldOnly(context, srcBotID);
			} else {
				getLogger().debug("Skipping Robot-ID {} because it doesn't match pattern {}", srcBotID, optRobotID_elseAllRobots);
			}
		}
	}

	private void startR50(BundleContext context) {
		Configuration<String> conf = R50ConfigUtils.getDefaultR50Config();
		conf.getPropertySetter(ROBOT_XML_CONFIG_PATH).handleEvent("/home/fit/robokind/resources/robot.xml");
		conf.getPropertySetter(JOINT_GROUP_XML_CONFIG_PATH).handleEvent("/home/fit/robokind/resources/jointgroup.xml");
		conf.getPropertySetter(VISEME_JSON_CONFIG_PATH).handleEvent("/home/fit/robokind/resources/VisemeConf.json");
		R50ConfigUtils.startR50(context, conf);
		//        startServicePanel(context);
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

	public void initLogging() {
		// Expects log4j.properties in the root of this bundle.
		// In Netbeans, look under "Other Sources"/"<default package>"
		// In the filesystem, look under src/main/resources
		File file = new File("log4j_hrk_dev.properties");
		//		logInfo("InstalledFileLocator resolved path[" + "" + "] in module[" + getVirtcharNBClusterDir() + "] to " + file.getAbsolutePath());
		if (file.exists()) {
			try {
				URL localURL = file.toURI().toURL();
				PropertyConfigurator.configure(localURL);
			} catch (MalformedURLException ex) {
				ex.printStackTrace();
			}
		}
		forceLog4jConfig();

		java.util.logging.Logger.getLogger(org.jflux.impl.services.rk.osgi.lifecycle.ServiceDependenciesTracker.class.getName()).setLevel(Level.WARNING);

	}

	boolean pumaConfiged = false;
	boolean pumaBooted = false;

	final public void configPuma() {

		boolean preventOverride = false;
		PumaContextMediator mediator = injectConfigBootstrapMediators(m_context, preventOverride);
		if (mediator == null)
			return;

		// startPumaDemo is intended to be self-contained in time, at least insofar as it is
		// now OK for applications to start their own puma-dependent features.  So at this
		// point, an app can fire up its own goodies, and tell them to attach to
		// PUMA  behavior system.  However, the Cogchar config system is intended to be
		// sufficiently general to handle most initialization cases without help from
		// bundle activators.

		getLogger().info(EQBAR + "\nCreating PumaBooter");
		// Cheaters context is used only for our demo-specific debugging features.
		// If another mediator took over instead, then we won't try to "cheat" to make those debugging features run.

		PumaContextMediator mediator2 = PumaGlobalPrebootInjector.getTheInjector().getMediator();
		PumaAppContext localDemoCheatersContext = null;

		if (mediator2 instanceof CommonMediator) {
			localDemoCheatersContext = ((CommonMediator) mediator2).myDemoPACtx;
		}

		if (localDemoCheatersContext != null) {
			getLogger().info("We have a cheater's Puma-App-Context, but we're not cheatin with it today");
		}
	}

	public void ensurePuma() {
		if (!pumaConfiged)
			configPuma();
		if (!pumaBooted)
			bootPuma();
	}

	final public void bootPuma() {
		PumaBooter pumaBooter = new PumaBooter();
		// Cheaters context is used only for our demo-specific debugging features.
		// If another mediator took over instead, then we won't try to "cheat" to make those debugging features run.

		PumaContextMediator mediator2 = PumaGlobalPrebootInjector.getTheInjector().getMediator();
		PumaAppContext localDemoCheatersContext = null;

		if (mediator2 instanceof CommonMediator) {
			localDemoCheatersContext = ((CommonMediator) mediator2).myDemoPACtx;
		}

		if (localDemoCheatersContext != null) {
			getLogger().info("We have a cheater's Puma-App-Context, but we're not cheatin with it today");
		}
		// Here is where the config actually gets read and used, then the OpenGL world gets created and populated.
		// The trickiest part is that there are several caveats about classloaders used for resources.
		// Those are what the Mediator pattern is meant to address, and that's why we set those up separately, first.
		PumaBooter.BootResult bootResult = pumaBooter.bootUnderOSGi(m_context, mediator2);
		getLogger().info(EQBAR + "\nGot PUMA BootResult: " + bootResult);
		// Uncomment/Comment this to enable/disable our demo of the "MotionComputer" computed-animation API.
		// Currently it plays a sinusoid on the waist-turn joint.
		// TODO:  Document, start hooking up to Symja and repo-config system.
	}

	protected Logger getLogger() {
		return super.getLogger();
	}

	final @Override
	protected void handleFrameworkStartedEvent(final BundleContext bundleCtx) {
		try {
			// Legitimate "application" processing begins now, after all bundles have loaded and
			// the whole framework has "started" as succesfully as it can.
			if (isLauncherBundle())
				launchApplication(bundleCtx);
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

	}

	protected PumaContextMediator injectConfigBootstrapMediators(final BundleContext context, boolean preventOverride) {
		PumaContextMediator mediatorFromLocalDisk = getMediator();
		if (mediatorFromLocalDisk == null)
			return null;
		getLogger().info(EQBAR + "\nSetting up two mediators: disk-based and online-sheet-based");
		// Create one or more mediators, which do not yet actually read from their sources.

		PumaGlobalPrebootInjector injector = PumaGlobalPrebootInjector.getTheInjector();
		// False => Do not overwrite, so any other customer mediator will get preference.
		// Our DemoMediator coded below is only used as a backup/default.
		injector.setMediator(mediatorFromLocalDisk, preventOverride);
		return mediatorFromLocalDisk;

	}

	protected void forceLog4jConfig() {
		super.forceLog4jConfig();
	}

	static Object clInit = new Object() {
		{
			doubug("<clinit>" + this.getClass());
			try {
				System.getProperties().store(System.out, "cliIntiTime");
			} catch (Exception e) {
			}
		}
	};

	public static boolean LAUNCH_MYSELF = true;
}
