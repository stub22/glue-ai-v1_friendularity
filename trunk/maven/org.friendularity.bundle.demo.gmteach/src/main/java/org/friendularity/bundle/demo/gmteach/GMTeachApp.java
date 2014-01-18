package org.friendularity.bundle.demo.gmteach;

import static org.friendularity.bundle.macro.tools.R50ConfigUtils.JOINT_GROUP_XML_CONFIG_PATH;
import static org.friendularity.bundle.macro.tools.R50ConfigUtils.ROBOT_XML_CONFIG_PATH;
import static org.friendularity.bundle.macro.tools.R50ConfigUtils.VISEME_JSON_CONFIG_PATH;

import java.awt.HeadlessException;
import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.rmi.activation.Activatable;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Level;

import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JScrollPane;
import javax.swing.UIManager;

import org.apache.log4j.PropertyConfigurator;
import org.appdapter.core.log.Debuggable;
import org.appdapter.core.matdat.EnhancedRepoClient;
import org.appdapter.core.matdat.RepoSpec;
import org.appdapter.core.name.FreeIdent;
import org.appdapter.core.name.Ident;
import org.appdapter.core.store.Repo;
import org.appdapter.gui.demo.DemoBrowser;
import org.appdapter.gui.demo.UISettings;
import org.appdapter.help.repo.RepoClient;
import org.cogchar.app.puma.boot.PumaAppContext;
import org.cogchar.app.puma.boot.PumaBooter;
import org.cogchar.app.puma.config.PumaContextMediator;
import org.cogchar.app.puma.registry.PumaGlobalPrebootInjector;
import org.cogchar.app.puma.web.LifterLifecycle;
import org.cogchar.bind.midi.out.CogcharMidiOutputTestMain;
import org.cogchar.bind.rk.behavior.SceneLifecycleDemo;
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
import org.cogchar.render.sys.module.RenderGateway;
import org.cogchar.render.trial.TrialBalloon;
import org.cogchar.svc.behav.control.BehaviorControlServiceManager;
import org.friendularity.bundle.macro.behavior.BehaviorMasterConfig;
import org.friendularity.bundle.macro.behavior.BehaviorMasterLifecycle;
import org.friendularity.bundle.macro.behavior.HeadlessBehaviorLifecycle;
import org.friendularity.bundle.macro.behavior.TriggerFrame;
import org.friendularity.bundle.macro.common.CommonActivator;
import org.friendularity.bundle.macro.common.CommonMediator;
import org.friendularity.bundle.macro.jvision.CCRK_DemoMediator;
import org.friendularity.bundle.macro.jvision.CCRK_DemoMotionComputer;
import org.friendularity.bundle.macro.tools.R50ConfigUtils;
import org.friendularity.gmteach.estimate.api.west.WorldEstimate;
import org.friendularity.gmteach.estimate.impl.visual.EstimateVisualizer;
import org.friendularity.gmteach.ext.midi.CogcharMidiInputTestMain;
import org.friendularity.gmteach.ext.symcalc.ScriptEngineExperiment;
import org.friendularity.gmteach.ext.vworld.VisionDataFeed;
import org.friendularity.gmteach.goal.GoalRegistery;
import org.friendularity.gmteach.recognizer.MidiEventRecognizer;
import org.friendularity.gmteach.recognizer.WorldEstimateRecognizer;
import org.friendularity.jvision.gui.JVisionLauncher;
import org.jflux.api.core.config.Configuration;
import org.jflux.api.core.config.DefaultConfiguration;
import org.jflux.impl.registry.OSGiRegistry;
import org.jflux.impl.services.rk.lifecycle.AbstractLifecycleProvider;
import org.jflux.impl.services.rk.lifecycle.ServiceLifecycleProvider;
import org.jflux.impl.services.rk.lifecycle.utils.DescriptorListBuilder;
import org.jflux.impl.services.rk.osgi.lifecycle.OSGiComponent;
import org.jflux.swing.messaging.monitor.AvroTableDemoFrame;
//import org.rwshop.swing.messaging.monitor.AvroTableDemoFrame;
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
import org.slf4j.Logger;
import org.storychat.bundle.glulx.Activator;
import org.storychat.game.StoryChatInterpretor;
import org.storychat.glulx.GlulxeMain;
import org.storychat.scripting.GInterpreter;
import org.zmpp.glulx.GlulxVM;
import org.zmpp.glulx.stdlib.Glulx;
import org.zmpp.glulx.stdlib.GlulxMain;
//import org.cogchar.bind.midi.FunMidiEventRouter;

import bsh.EvalError;

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
public class GMTeachApp extends CommonActivator implements BundleListener, ServiceListener {
	private static GMTeachApp gmStaticIsntance = new GMTeachApp();

	public GMTeachApp() {
		gmStaticIsntance = this;
	}

	MidiEventRecognizer myMidiEventRecognizer = new MidiEventRecognizer(this);
	GoalRegistery myGoalRegistery = new GoalRegistery(this);
	WorldEstimateRecognizer estimateRecognizer = new WorldEstimateRecognizer(this);
	static MockBundleContext fakeBundleCtx = new MockBundleContext();

	//	WorldEstimateRecognizer myEstimateRecognizer = new WorldEstimateRecognizer();

	public static void main(String[] args) throws Exception {
		gmStaticIsntance = new GMTeachApp();
		gmStaticIsntance.args = args;
		gmStaticIsntance.start(fakeBundleCtx);
		gmStaticIsntance.launchDemoApp();
	}

	public void reportObject(Object ime) {
		if (ime == null) {
		} else if (ime instanceof String) {
			submitEvent((String) ime, null);
		} else if (ime instanceof EventAngifiable) {
			submitEvent(((EventAngifiable) ime).getEnglishEvent(), ime);
		} else {
			Class c = ime.getClass();
			EventAngifiable eng = getObjectEnglishEncoder(c, ime);
			if (eng != null) {
				submitEvent(eng.getEnglishEvent(), eng);
			} else {
				submitEvent(Debuggable.toInfoStringO(ime), ime);
			}
		}

	}

	private EventAngifiable getObjectEnglishEncoder(Class c, Object o) {
		return null;
	}

	private void submitEvent(String ime, Object obj) {
		StoryChatInterpretor sci = getStoryChatInterpretor();
		if (sci == null)
			return;
		sci.submitEvent(ime, obj, this);
	}

	private StoryChatInterpretor getStoryChatInterpretor() {
		GInterpreter pareGInterpreter = Glulx.ensureParentInterpreter();
		try {
			Object o = pareGInterpreter.get("glkf");
			if (o == null)
				return null;
			return (StoryChatInterpretor) o;
		} catch (EvalError e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			return null;
		}
	}

	LinkedList<Runnable> shutdownHooksRunnables = new LinkedList<Runnable>();

	public void addShutdownHook(Runnable runnable) {
		synchronized (shutdownHooksRunnables) {
			shutdownHooksRunnables.add(runnable);
		}

	}

	public static GMTeachApp staticInstance() {
		return gmStaticIsntance;
	}

	PumaContextMediator myMediator = new CCRK_DemoMediator();

	protected boolean isHeadless() {
		return HEADLESS;
	}

	@Override public void start(BundleContext bundleCtx) throws Exception {
		myMediator = null;//new CCRK_DemoMediator();
		m_context = bundleCtx;
		HEADLESS = false;
		MACRO_LAUNCHER = false;
		Activator.AUTO_LAUNCH = false;
		// TODO Auto-generated method stub
		super.start(bundleCtx);
		super.bundleBootPhase = BootPhaseConst.UNSTARTED;
		if (!MACRO_LAUNCHER) {
			getLogger().error("org.friendularity.bundle.macrolauncher was not preceeded by org.friendularity.bundle.macro.common");
		}
		if (bundleCtx != null) {
			bundleCtx.removeBundleListener(this);
			bundleCtx.addBundleListener(this);
			bundleCtx.removeServiceListener(this);
			bundleCtx.addServiceListener(this);
		}
		setupDefaultConfigs(bundleCtx);

		// take ownership as the primary bundle
		macroStartupSettings.makeStartupBundle(this);
		registerServices(bundleCtx);
		//macroStartupSettings.raiseToPhase(MacroStartupSettings.COMPLETED_REGISTERSERVICES);
		macroStartupSettings.runNow("configPuma");
		macroStartupSettings.printMacroStateInfo(System.err, null, true);
		macroStartupSettings.removeBegun();
		configGMTeachStory();
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

	@Override public void serviceChanged(ServiceEvent event) {
		System.err.println("serviceChanged event " + event);

	}

	@Override public void bundleChanged(BundleEvent event) {
		System.err.println("bundle event " + event);
	}

	void setDefaultSettings(boolean startAll) {
		putSetting("connectJVision", startAll);
		putSetting("connectMidiIn", startAll);
		putSetting("connectMidiOut", startAll);
		putSetting("whackamole", true);
		// routeCamera will break the 2D stuff
		putSetting("routeCamera", false);
		putSetting("monitorLifecycles", startAll);
		putSetting("VirtualWorld", startAll);
		putSetting("Behavior", startAll);
		putSetting("AvroMessages", startAll);
		putSetting("BootPuma", startAll);
		putSetting("DeicticVisualizer", startAll);
	}

	protected void putSetting(String key, boolean value) {
		macroStartupSettings.putSetting(key, value);

	}

	protected void setupDefaultConfigs(final BundleContext context) {
		setDefaultSettings(true);
		// No longer Have to set this bool early in start() here
		// org.friendularity.bundle.jvision.JVisionBundleActivator.LAUNCH_MYSELF = false;// flagTrue("connectJVision");
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

	boolean launchedOnce = false;
	Object launchedOnceLock = new Object();
	private String[] args = new String[0];

	public void launchApplication(BundleContext context) throws Exception {
		synchronized (launchedOnceLock) {
			if (launchedOnce)
				return;
			launchedOnce = true;
		}
		if (this.bundleBootPhase < BootPhaseConst.LAUNCHING) {
			this.bundleBootPhase = BootPhaseConst.LAUNCHING;
			macroStartupSettings.makeStartupBundle(this);

			launchDemoApp();
			this.bundleBootPhase = BootPhaseConst.LAUNCHING_COMPLETE;
		}
	}

	public void launchDemoApp() {
		new Thread(new Runnable() {
			@Override public void run() {
				try {
					runDemoApp();
				} catch (Exception e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
		}, "launchDemoApp " + this).start();
	}

	public void runDemoApp() throws Exception {
		showDemoAppStarter();
		try {
			getLogger().warn("runDemoApp Activation in 10 seconds.");
			//Thread.sleep(10000);
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		// How to get the cmd line args if we need them
		getLogger().warn("gmteach.glulx Activation in 0 seconds.");
		// raise to DURRING_CONFIG
		macroStartupSettings.raiseToPhase(MacroStartupSettings.DURRING_CONFIG);
		configGMTeachStory();
		// raise to POST_CONFIG
		macroStartupSettings.raiseToPhase(MacroStartupSettings.POST_CONFIG);
		// show POST_CONFIG
		macroStartupSettings.printMacroStateInfo(System.err, m_context, false);
		getLogger().warn("launchPhases in 0 seconds.");
		macroStartupSettings.launchPhases();
		macroStartupSettings.removeBegun();
		getLogger().warn("launchPhases COMPLETE.");
		showMacroFrame();
		myMidiEventRecognizer.init(args);
		myGoalRegistery.init(args);
		estimateRecognizer.initModule();
		macroStartupSettings.runNow("ccrk");
	}

	private void configGMTeachStory() {
		// add our stuff
		Bundle ourBundle = getBundle();
		String args = macroStartupSettings.getProperty(ourBundle, "src/main/Inform/Projects/GMTeach.inform/Build/output.ulx", "gmteach.glulx", "inform7.story", "glulx.args", "application.args", "launcher.arguments").trim();
		macroStartupSettings.putSetting("inform7.story", args);
	}

	private Bundle getBundle() {
		if (m_context != null) {
			return m_context.getBundle();
		}
		return null;
	}

	private void showMacroFrame() {
		macroStartupSettings.removeBegun();
		if (isHeadless())
			return;
		JFrame shownIn = new JFrame("macroStartupSettings");
		shownIn.add(new JScrollPane(new RunnableComponentPanel(macroStartupSettings)));
		shownIn.pack();
		shownIn.show(true);

	}

	public void showDemoAppStarter() throws HeadlessException {
		DemoBrowser.appName = "MacroLauncher";
		DemoBrowser.addRepoLoaderMenu();
		showObject("macroStartupSettings", macroStartupSettings, false, false);
		showObject("macroActionCallbackMap", macroStartupSettings.actionCallbackMap, false, false);
		org.appdapter.gui.browse.Utility.setSingletonValue(BundleContext.class, m_context);
		if (isHeadless())
			return;
		DemoBrowser.show();
	}

	static void showObject(String name, Object value, final boolean showASAP, final boolean loadChildren) {
		GInterpreter pareGInterpreter = Glulx.ensureParentInterpreter();
		pareGInterpreter.setGlobally(name, value);
		if (value == null) {
			throw new NullPointerException(name);
		}
		DemoBrowser.showObject(name, value, showASAP, loadChildren);

	}

	protected PumaContextMediator getMediator() {
		synchronized (pumaBootLock) {
			if (myMediator == null) {
				myMediator = new CommonMediator(m_context);
			}
			return myMediator;
		}
	}

	static class AnimChanLifecycle extends AbstractLifecycleProvider {
		protected List<ClassLoader> myResourceClassLoaders;

		public AnimChanLifecycle(String robotId, List<ClassLoader> resourceClassLoaders) {
			super(new DescriptorListBuilder().dependency("animPlayer", AnimationPlayer.class).with(AnimationPlayer.PROP_PLAYER_ID, robotId).dependency("repoClient", RepoClient.class).getDescriptors());
			if (myRegistrationProperties == null) {
				myRegistrationProperties = new Properties();
			}
			myRegistrationProperties.put("thingActionChannelAgentId", robotId);
			myResourceClassLoaders = resourceClassLoaders;
		}

		@Override public synchronized Object getService() {
			return super.getService();
		}

		protected Object create(Map dependencies) {
			RepoClient rc = (RepoClient) dependencies.get("repoClient");
			BehaviorConfigEmitter bce = new BehaviorConfigEmitter(rc, rc.makeIdentForQName(AnimFileSpecReader.animGraphQN()));
			AnimationPlayer player = (AnimationPlayer) dependencies.get("animPlayer");
			RobotAnimContext rac = new RobotAnimContext(new FreeIdent("http://hrkind.com/robot#zenoR50"), bce);
			rac.setResourceClassLoaders(myResourceClassLoaders);
			rac.initConnForAnimPlayer(player);
			return rac.getTriggeringChannel();
		}

		@Override protected Class<Object> getServiceClass() {
			return Object.class;
		}

		protected void handleChange(String name, Object dependency, Map availableDependencies) {
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

	WorldEstimateRecognizer werm0 = null;
	EstimateVisualizer<?> eViz;
	protected MathGateUnscripted mg;
	protected WorldEstimate we;
	protected MidiEventRecognizer myMidiMapper = new MidiEventRecognizer(GMTeachApp.staticInstance());

	public void registerServices(final BundleContext context0) {
		if (m_context == null)
			m_context = context0;

		addMacroPreService("useBehaviorRepoConfig", new Runnable() {
			@Override public void run() {
				installSharedRepo(m_context);
			}
		});

		addMacroPreService("configPuma", new Runnable() {
			@Override public void run() {
				configPuma();
			}
		});

		addMacroServiceButton("setLookAndFeel", new Runnable() {
			@Override public void run() {
				setLookAndFeel();
			}
		});

		addMacroServiceButton("monitorLifecycles", new Runnable() {
			@Override public void run() {
				startServicePanel(m_context);
			}
		});

		addMacroService("bootPuma", new Runnable() {
			@Override public void run() {
				ensurePuma();
			}
		});

		addMacroServiceButton("ccrk", new Runnable() {
			@Override public void run() {
				startPumaDemo(context0);
			}
		});

		addMacroServiceButton("worldEstimate", new Runnable() {
			@Override public void run() {
				macroStartupSettings.ensureReady("bootPuma");
				ensureWerm();
			}
		});
		addMacroServiceButton("attachVWorldRenderModule", new Runnable() {
			@Override public void run() {
				macroStartupSettings.ensureReady("worldEstimate");
				WorldEstimateRecognizer werm = ensureWerm();
				PumaAppUtils.attachVWorldRenderModule(m_context, werm, null);
			}
		});
		addMacroServiceButton("setupVisualizer", new Runnable() {
			@Override public void run() {
				macroStartupSettings.ensureReady("attachVWorldRenderModule");
				if (isHeadless())
					return;
				WorldEstimateRecognizer werm = ensureWerm();

				RenderGateway rg = werm.getRenderGateway();
				if (rg != null) {
					getLogger().error("werm has no RenderGateway");
				}
				eViz = werm.setupVisualizer(null, null, null);
			}
		});

		addMacroServiceButton("makeUnscriptedMathGate", new Runnable() {
			@Override public void run() {
				macroStartupSettings.ensureReady("worldEstimate");
				// Needs to be done at least once for the selfEstim to exist.
				MathSpaceFactory msf = new MathSpaceFactory();
				// MathGate mg = msf.makeScriptedMathGate();
				GMTeachApp.this.mg = msf.makeUnscriptedMathGate();
				WorldEstimateRecognizer werm = ensureWerm();
				werm.setMathGate(mg);

			}
		});
		addMacroServiceButton("world_estim_31", new Runnable() {
			@Override public void run() {
				macroStartupSettings.ensureReady("attachVWorldRenderModule");
				Ident worldEstimID = new FreeIdent(WorldEstimate.ESTIM_NS + "world_estim_31");
				we = new WorldEstimate(worldEstimID);
				WorldEstimateRecognizer werm = ensureWerm();
				werm.setWorldEstimate(we);
			}
		});

		addMacroServiceButton("startMotionComputers", new Runnable() {
			@Override public void run() {
				macroStartupSettings.ensureReady("world_estim_31");
				Robot.Id optRobotID_elseAllRobots = null;
				startMotionComputers(m_context, optRobotID_elseAllRobots, we);
			}
		});

		addMacroServiceButton("connectJVision", new Runnable() {
			@Override public void run() {
				//	Startup the optional JVision connection
				startVisionMonitors();
			}
		});

		addMacroServiceButton("connectMidiIn", new Runnable() {
			@Override public void run() {
				WorldEstimateRecognizer werm = ensureWerm();
				myMidiMapper.startMidiRouters(werm);
			}
		});

		addMacroServiceButton("connectMidiOut", new Runnable() {
			@Override public void run() {
				myMidiMapper.startMidiOutputDemo();
			}
		});

		addMacroServiceButton("connectMidiSwitcheroo", new Runnable() {
			@Override public void run() {
				myMidiMapper.startMidiSwitcherooDemo();
			}
		});

		addMacroServiceButton("whackamoleReloadObjects", new Runnable() {
			@Override public void run() {
				if (isHeadless())
					return;
				setupDebuggingScaffold(mg, we);
			}
		});

		addMacroServiceButton("BehaviorControlServiceManager", new Runnable() {
			@Override public void run() {
				BehaviorControlServiceManager.init(new OSGiRegistry(m_context));
			}
		});

		addMacroServiceButton("SpeechRecognitionServiceManager", new Runnable() {
			@Override public void run() {
				// Still TODO
				//	SpeechRecognitionServiceManager
				//		.init(new OSGiRegistry(m_context));
			}
		});

		addMacroServiceButton("R50ConfigUtils.startR50", new Runnable() {
			@Override public void run() {
				startR50(m_context);
			}
		});

		addMacroServiceButton("initLifterWebappLifecycle", new Runnable() {
			@Override public void run() {
				initLifterWebappLifecycle(m_context);
			}
		});

		addMacroServiceButton("SceneLifecycleDemoTest", new Runnable() {
			@Override public void run() {
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

		addMacroServiceButton("AnimChanLifecycle", new Runnable() {
			@Override public void run() {
				myMediator = getMediator();
				List<ClassLoader> classLoaders = new ArrayList<ClassLoader>();
				if (myMediator instanceof CommonMediator)
					classLoaders.addAll(((CommonMediator) myMediator).getFileResClassLoaders());
				new OSGiComponent(m_context, new AnimChanLifecycle("myRobot", classLoaders)).start();
			}
		});

		addMacroServiceButton("startSillyMotionComputersDemoForVWorldOnly", new Runnable() {
			@Override public void run() {
				Robot.Id optRobotID_elseAllRobots = null;
				PumaAppUtils.startSillyMotionComputersDemoForVWorldOnly(m_context, optRobotID_elseAllRobots);
			}
		});

		addMacroServiceButton("checkAnimFiles", new Runnable() {
			@Override public void run() {
				PumaAppUtils.checkAnimationFiles(null);
			}
		});

		/*
		addMacroService("deicticVisualizer", new Runnable() {
			public void run() {
				DeicticVisualizer deictViz = new DeicticVisualizer();
				deictViz.forceHeadCameraOntoSinbad();
			}
		});
		*/
		addMacroServiceButton("TrialBalloon", new Runnable() {
			@Override public void run() {
				TrialBalloon.main(new String[0]);
			}
		});

		addMacroServiceButton("ScriptEngineExperiment", new Runnable() {
			@Override public void run() {
				ScriptEngineExperiment.main(new String[0]);
			}
		});

		addMacroServiceButton("startQPIDMonitor", new Runnable() {
			@Override public void run() {
				startQPIDMonitor(m_context);
			}
		});

		addMacroServiceButton("setupRepoUpdateCallback", new Runnable() {

			@Override public void run() {
				setupRepoUpdateCallback();
			}
		});

		//************************************************************************************************
		// With this call enabled, we get a Permgen during Oglweb.r25 init on JDK6-Win7x64 with -XX:MaxPermSize=512M.
		// With it disabled, we are hunky-dory?
		addMacroService("launchBehaviorLifecycles", new Runnable() {
			@Override public void run() {
				launchBehaviorLifecycles(m_context);
			}
		});

		//************************************************************************************************

		addMacroServiceButton("launchTriggerPanel", new Runnable() {
			@Override public void run() {
				startTriggerPanelSeparately(m_context);
			}
		});

		addMacroServiceButton("addConfigDemo", new Runnable() {
			@Override public void run() {

				addConfigs(m_context);
			}
		});

		addMacroServiceButton("CogcharMidiInputTestMain", new Runnable() {
			@Override public void run() {
				CogcharMidiInputTestMain.main(new String[0]);
			}
		});

		addMacroServiceButton("CogcharMidiOutputTestMain", new Runnable() {
			@Override public void run() {
				CogcharMidiOutputTestMain.main(new String[0]);
			}
		});
	}

	public void addMacroServiceButton(String key, Runnable runnable) {
		addMacroService(BootPhaseConst.ON_DEMAND, key, runnable);
	}

	private void startPumaDemo(BundleContext bundleCtx) {

		macroStartupSettings.runNow("bootPuma");

		// Hey, let's get some fused-sensor-data visualization going too, while we're at it!

		WorldEstimateRecognizer werm = ensureWerm();

		// Enable/Disable this texture flow based on whether we are launching JVision or not.
		// Should be a dont-care whether this happens before/after   startVisionMonitors() below.
		// TODO:  Re-verify in detail.
		try {
			werm.setFlag_visionTextureRoutingEnabled(isEnabled("visionMonitors") && false);
		} catch (Throwable t) {
		}

		PumaAppUtils.attachVWorldRenderModule(bundleCtx, werm, null);
		eViz = werm.setupVisualizer(null, null, null);
		// Needs to be done at least once for the selfEstim to exist.
		//MathSpaceFactory msf = new MathSpaceFactory();
		// MathGate mg = msf.makeScriptedMathGate();
		macroStartupSettings.runNow("makeUnscriptedMathGate");
		macroStartupSettings.runNow("world_estim_31");
		macroStartupSettings.runNow("startMotionComputers");

		if (isEnabled("visionMonitors")) {

			//	Startup the optional JVision connection
			try {
				macroStartupSettings.runNow("connectJVision");
			} catch (Throwable t) {
				t.printStackTrace();
			}
		}

		macroStartupSettings.runNow("connectMidiIn");
		macroStartupSettings.runNow("connectMidiOut");
		macroStartupSettings.runNow("connectMidiOut");

		if (isEnabled("connectSwingDebugGUI")) {
			setupDebuggingScaffold(mg, we);
		}
	}

	protected boolean isEnabled(String string) {
		return macroStartupSettings.isEnabled(string);
	}

	protected WorldEstimateRecognizer ensureWerm() {
		synchronized (pumaBootLock) {
			if (werm0 == null) {
				// Hey, let's get some fused-sensor-data visualization going too, while we're at it!
				werm0 = new WorldEstimateRecognizer(this);
				// Enable/Disable this texture flow based on whether we are launching JVision or not.
				// Should be a dont-care whether this happens before/after   startVisionMonitors() below.
				// TODO:  Re-verify in detail.
				werm0.setFlag_visionTextureRoutingEnabled(macroStartupSettings.flagTrue("routeCamera"));
			}
			return werm0;
		}
	}

	JVisionLauncher myLauncher = null;

	protected void launchJVisionDemo() {
		// How to get the cmd line args if we need them
		// String[] args = (String[])context.getArguments().get("application.args");

		// Check that our Java version is at least Java 1.6 update-32
		// Versions at u25 and earlier fail with a "can't find native-library" error.
		// 1.6.0_32
		String version = System.getProperty("java.version");

		if (version == null) {
			getLogger().error("Cannot determine java version, we need 1.6.0_32, will try to run anyway");
		} else if (!version.equals("1.6.0_32")) {
			getLogger().warn("Java version is not 1.6.0_32, Versions at u25 and "
					+ "earlier fail with a \"can't find native-library\" error.");
		}

		boolean flag_stopOSGiAfterQuitCompletes = true;
		myLauncher = new JVisionLauncher(flag_stopOSGiAfterQuitCompletes);
		boolean launchedOK = myLauncher.attemptInit();
		getLogger().info("myLauncher.attemptInit() returned: " + launchedOK);
	}

	protected void addConfigs(BundleContext context) {

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

	protected void launchBehaviorLifecycles(BundleContext context) {

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

	protected void installSharedRepo(BundleContext context) {

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

	protected void setLookAndFeel() {
		javax.swing.SwingUtilities.invokeLater(new Runnable() {
			@Override public void run() {
				try {
					if (isHeadless())
						return;
					if (isEnabled("systemLAF") && false) {
						UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
					}
					else {
						UIManager.setLookAndFeel(UIManager.getCrossPlatformLookAndFeelClassName());
					}
				} catch (Exception ex) {
				}
			}
		});
	}

	// made public for outside control
	public void startServicePanel(final BundleContext context) {
		java.awt.EventQueue.invokeLater(new Runnable() {
			@Override public void run() {
				if (isHeadless())
					return;
				ServicesFrame.create(context);
			}
		});
	}

	// made public for outside control
	public void startTriggerPanelSeparately(final BundleContext context) {
		java.awt.EventQueue.invokeLater(new Runnable() {
			@Override public void run() {
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

	protected void setupDebuggingScaffold(MathGate mg, WorldEstimate we) {
		UISettings uiSettings = DemoBrowser.getSettings();
		uiSettings.setOverzealousHunter(true);
		showObject("werm-MG", mg, false, false); // true, true);
		if (we != null)
			showObject("amazingly accurate estimate", we, false, false);
		PumaAppUtils.GreedyHandleSet greedyHandles = PumaAppUtils.obtainGreedyHandleSet();
		showObject("our-greedy-handles", greedyHandles, false, false);
		showObject("our-repo-client", greedyHandles.rc, false, false);
		DemoBrowser.show();
	}

	public void setupRepoUpdateCallback() {
		RepoUpdateCallbackAdapter.registerCallback(new RepoUpdateCallbackAdapter.Callback() {
			public void repoUpdateCompleted() {
				getLogger().info("c.h.b.oglweb.R50 activator got SPARQL-UPDATE callback");
				GruesomeTAProcessingFuncs.processPendingThingActions();
			}
		});
	}

	/**
	 * For each joint robot, robokind.org blends all joint inputs received from RobotMoverFrameSources. Cogchar.org defines the CogcharMotionSource subclass. A cogcharMotionSource has an ordered list of CogcharMotionComputers.
	 * 
	 * @param context
	 * @param optRobotID_elseAllRobots
	 */
	protected void startMotionComputers(BundleContext context, Robot.Id optRobotID_elseAllRobots, WorldEstimate we) {
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

	protected void startR50(BundleContext context) {
		Configuration<String> conf = R50ConfigUtils.getDefaultR50Config();
		conf.getPropertySetter(ROBOT_XML_CONFIG_PATH).handleEvent("/home/fit/robokind/resources/robot.xml");
		conf.getPropertySetter(JOINT_GROUP_XML_CONFIG_PATH).handleEvent("/home/fit/robokind/resources/jointgroup.xml");
		conf.getPropertySetter(VISEME_JSON_CONFIG_PATH).handleEvent("/home/fit/robokind/resources/VisemeConf.json");
		R50ConfigUtils.startR50(context, conf);
		//        startServicePanel(context);
	}

	protected void startVisionMonitors() {
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

	static boolean pumaConfiging = false;
	static boolean pumaBooting = false;
	static Object pumaBootLock = new Object();

	final public void ensurePuma() {
		synchronized (pumaBootLock) {
			if (!pumaConfiging)
				configPuma();
			if (!pumaBooting)
				bootPuma();

		}
	}

	final public void configPuma() {
		synchronized (pumaBootLock) {
			if (!pumaConfiging) {
				pumaConfiging = true;
				configPuma0();
			}
		}
	}

	private void configPuma0() {

		// Register our default mediator
		boolean preventOverride = false;
		PumaContextMediator mediatorFromLocalDisk = getMediator();
		getLogger().info(EQBAR + "\nSetting up two mediators: disk-based and online-sheet-based");
		// Create one or more mediators, which do not yet actually read from their sources.

		PumaGlobalPrebootInjector injector = PumaGlobalPrebootInjector.getTheInjector();
		// False => Do not overwrite, so any other customer mediator will get preference.
		// Our DemoMediator coded below is only used as a backup/default.
		injector.setMediator(mediatorFromLocalDisk, preventOverride);

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

	/*
	static Object global_lock;

	public static synchronized Object getGlobalLock() {
		if (global_lock == null) {
			try {
				global_lock = AccessController.doPrivileged(new PrivilegedExceptionAction<Object>() {
					public Object run() throws Exception {
						Field lock_field = Class.forName("org.lwjgl.opengl.GlobalLock").getDeclaredField("lock");
						lock_field.setAccessible(true);
						return lock_field.get(null);
					}
				});
			} catch (PrivilegedActionException e) {
				throw new Error(e);
			}
		}
		return global_lock;
	}*/

	final public void bootPuma() {
		synchronized (pumaBootLock) {
			if (!pumaBooting) {
				pumaBooting = true;
				bootPuma0();
			}
		}
	}

	private void bootPuma0() {

		PumaBooter pumaBooter = new PumaBooter();
		PumaContextMediator mediator = PumaGlobalPrebootInjector.getTheInjector().getMediator();
		if (isFakeOSGI()) {
			PumaBooter.BootResult bootResult = pumaBooter.bootUnderOSGi(m_context, mediator);
			getLogger().info("Got PUMA BootResult: " + bootResult);

		} else {
			PumaBooter.BootResult bootResult = pumaBooter.bootUnderOSGi(m_context, mediator);
			getLogger().info("Got PUMA BootResult: " + bootResult);
		}
		// TODO:  Pay attention to {the set of relevant charIDs and component configs}, as we set up these
		// motionComputer + estimateVisualizer components.

		// Cheaters context is available locally only for our demo-specific debugging features.
		// [It is currently unused]
		// If another mediator took over instead, then we won't try to "cheat" to make those debugging features run.
		PumaAppContext localDemoCheatersContext = null;
		if (mediator instanceof CCRK_DemoMediator) {
			localDemoCheatersContext = ((CCRK_DemoMediator) mediator).myDemoPACtx;
		}

		if (localDemoCheatersContext != null) {
			getLogger().info("We have a cheater's Puma-App-Context, but we're not cheatin with it today - hooray!");
		}

	}

	protected Logger getLogger() {
		return super.getLogger();
	}

	final @Override protected void handleFrameworkStartedEvent(final BundleContext bundleCtx) {
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
