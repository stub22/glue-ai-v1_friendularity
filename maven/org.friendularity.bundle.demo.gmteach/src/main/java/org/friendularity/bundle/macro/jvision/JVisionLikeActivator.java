package org.friendularity.bundle.macro.jvision;

import java.lang.reflect.Method;
import java.util.List;
import java.util.Map;
import java.util.Properties;

import org.appdapter.core.name.FreeIdent;
import org.appdapter.core.name.Ident;
import org.appdapter.gui.demo.DemoBrowser;
import org.appdapter.help.repo.RepoClient;
import org.cogchar.api.thing.WantsThingAction;
import org.cogchar.app.puma.web.LifterLifecycle;
import org.cogchar.bind.midi.FunMidiEventRouter;
import org.cogchar.bind.rk.behavior.SceneLifecycleDemo;
import org.cogchar.bind.rk.robot.client.AnimOutTrigChan;
import org.cogchar.bind.rk.robot.client.RobotAnimContext;
import org.cogchar.bind.rk.robot.motion.CogcharMotionSource;
import org.cogchar.bind.symja.MathGate;
import org.cogchar.bind.symja.MathSpaceFactory;
import org.cogchar.blob.emit.BehaviorConfigEmitter;
import org.cogchar.bundle.app.puma.GruesomeTAProcessingFuncs;
import org.cogchar.bundle.app.puma.PumaAppUtils;
import org.cogchar.impl.channel.AnimFileSpecReader;
import org.cogchar.joswrap.RepoUpdateCallbackAdapter;
import org.friendularity.bundle.macro.common.CommonActivator;
import org.friendularity.bundle.macro.common.CommonMediator;
import org.friendularity.gmteach.api.west.EstimateVisualizer;
import org.friendularity.gmteach.api.west.WorldEstimate;
import org.friendularity.gmteach.api.west.WorldEstimateRenderModule;
import org.friendularity.gmteach.impl.visual.BonusVisualizer;
import org.friendularity.gmteach.vworld.VisionDataFeed;
import org.jflux.impl.services.rk.lifecycle.AbstractLifecycleProvider;
import org.jflux.impl.services.rk.lifecycle.utils.DescriptorListBuilder;
import org.jflux.impl.services.rk.osgi.lifecycle.OSGiComponent;
import org.osgi.framework.BundleContext;
import org.robokind.api.animation.player.AnimationPlayer;
import org.robokind.api.motion.Robot;
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
 * The actual subset is determined through the intervation of a customizable PumaContextMediator object.
 * 
 * L1) This bundle does not try to start a lifter webapp, which is currently initialized orthogonally to the PUMA system. See the o.f.b.demo.liftoff project.
 * 
 * To exit, a user may X-closes our "main" simulator window, which calls stop on bundle 0.
 * 
 * @author Stu B. <www.texpedient.com>
 */
public class JVisionLikeActivator extends CommonActivator {

	private void startLifecycleMonitorGuiWindow(final BundleContext context) {
		ServicesFrame.create(context);
	}

	/** Not currently needed */
	private void startWhackamoleGuiWindow(final BundleContext context) {
		DemoBrowser.show();
	}

	public void registerServices(BundleContext context0) {

		addMacroService("ServicesFrame", new Runnable() {
			@Override
			public void run() {
				startLifecycleMonitorGuiWindow(m_context);
			}
		});

		addMacroPreService("configPuma", new Runnable() {
			@Override
			public void run() {
				configPuma();
			}
		});

		addMacroService("worldEstimate", new Runnable() {
			@Override
			public void run() {
				bootPuma();
				// Hey, let's get some fused-sensor-data visualization going too, while we're at it!
				final WorldEstimateRenderModule werm = new WorldEstimateRenderModule();

				// Enable/Disable this texture flow based on whether we are launching JVision or not.
				// Should be a dont-care whether this happens before/after   startVisionMonitors() below.
				// TODO:  Re-verify in detail.
				werm.setFlag_visionTextureRoutingEnabled(macroStartupSettings
						.flagTrue("connectJVision"));

				PumaAppUtils.attachVWorldRenderModule(m_context, werm, null);
				final EstimateVisualizer eViz = werm.setupVisualizer(null,
						null, null);
				// Needs to be done at least once for the selfEstim to exist.
				MathSpaceFactory msf = new MathSpaceFactory();
				// MathGate mg = msf.makeScriptedMathGate();
				final MathGate mg = msf.makeUnscriptedMathGate();
				werm.setMathGate(mg);
				Ident worldEstimID = new FreeIdent(WorldEstimate.ESTIM_NS
						+ "world_estim_31");

				final WorldEstimate we = new WorldEstimate(worldEstimID);
				werm.setWorldEstimate(we);

				Robot.Id optRobotID_elseAllRobots = null;
				startMotionComputers(m_context, optRobotID_elseAllRobots, we);

				addMacroService("connectMidiIn", new Runnable() {
					@Override
					public void run() {
						startMidiRouters(werm);
					}
				});
				addMacroService("connectSwingDebugGUI", new Runnable() {
					@Override
					public void run() {
						setupDebuggingScaffold(mg, we);
					}
				});

				addMacroService("sd", new Runnable() {
					@Override
					public void run() {
						startDeicticVisualizer((BonusVisualizer) eViz);
					}
				});

			}
		});

		addMacroService("deicticVisualizer", new Runnable() {
			public void run() {
				DeicticVisualizer deictViz = new DeicticVisualizer();
				deictViz.forceHeadCameraOntoSinbad();
			}
		});

		addMacroService("connectJVision", new Runnable() {
			@Override
			public void run() {
				//	Startup the optional JVision connection
				startVisionMonitors();
			}
		});
		addMacroService("ScriptEngineExperiment", new Runnable() {
			@Override
			public void run() {
				try {
					Method m = Class.forName(
							"org.cogchar.test.symcalc.ScriptEngineExperiment")
							.getMethod("main", new Class[] { String[].class });
					m.invoke(null, new Object[] { new String[0] });
				} catch (Exception e) {
					e.printStackTrace();
				}
				//ScriptEngineExperiment.main(null);
			}
		});

		addMacroService("whackamole", new Runnable() {
			@Override
			public void run() {
				startWhackamoleGuiWindow(m_context);
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

		addMacroService("ServicesFrame", new Runnable() {
			@Override
			public void run() {
				startServicePanel(m_context);
			}
		});

		addMacroService("setupRepoUpdateCallback", new Runnable() {
			@Override
			public void run() {
				setupRepoUpdateCallback();
			}
		});

	}

	private void startDeicticVisualizer(BonusVisualizer bViz) {

		DeicticVisualizer deictViz = new DeicticVisualizer();
		deictViz.connectToTrialContent(bViz.getTrialContent());
		deictViz.forceHeadCameraOntoSinbad();
		deictViz.putVizPyramidOnDefaultCam();
		deictViz.setupNiftyPointingRays();
		bViz.myDVHackForUpdate = deictViz;
	}

	private void setupDebuggingScaffold(MathGate mg, WorldEstimate we) {
		DemoBrowser.showObject("werm-MG", mg, false, false); // true, true);
		DemoBrowser.showObject("amazingly accurate estimate", we, false, false);
		PumaAppUtils.GreedyHandleSet greedyHandles = PumaAppUtils
				.obtainGreedyHandleSet();
		DemoBrowser.showObject("our-greedy-handles", greedyHandles, false,
				false);
		DemoBrowser.showObject("our-repo-client", greedyHandles.rc, false,
				false);
	}

	/**
	 * For each joint robot, robokind.org blends all joint inputs received from RobotMoverFrameSources. Cogchar.org defines the CogcharMotionSource subclass. A cogcharMotionSource has an ordered list of CogcharMotionComputers.
	 * 
	 * @param context
	 * @param optRobotID_elseAllRobots
	 */
	private void startMotionComputers(BundleContext context,
			Robot.Id optRobotID_elseAllRobots, WorldEstimate we) {
		List<CogcharMotionSource> cogMotSrcList = CogcharMotionSource
				.findCogcharMotionSources(context, optRobotID_elseAllRobots);

		for (CogcharMotionSource cms : cogMotSrcList) {
			Robot srcBot = cms.getRobot();
			Robot.Id srcBotID = srcBot.getRobotId();
			if ((optRobotID_elseAllRobots == null)
					|| optRobotID_elseAllRobots.equals(srcBotID)) {
				getLogger()
						.info("Found CogcharMotionSource for Robot-ID {} matching pattern {}",
								srcBotID, optRobotID_elseAllRobots);
				// Start a motion computer implemented locally in demo CCRK - tries to swing Sinbad around his spine
				DemoMotionComputer dmc = new DemoMotionComputer();
				dmc.setWorldEstimate(we);
				cms.addJointComputer(dmc);
				// append a trivial Sinbad-waist-sinusoid-demo implemented in CC-Puma.  Because it acts last, it has last
				// word, but should not unnecessarily override joint-pos from "earlier" phases=computers.
				// PumaAppUtils.startSillyMotionComputersDemoForVWorldOnly(context, srcBotID);
			} else {
				getLogger()
						.debug("Skipping Robot-ID {} because it doesn't match pattern {}",
								srcBotID, optRobotID_elseAllRobots);
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

	private void startMidiRouters(WorldEstimateRenderModule werm) {
		FunMidiEventRouter fmer = new FunMidiEventRouter();
		MidiCommandMapper mcm = new MidiCommandMapper();
		mcm.myWERM = werm;
		fmer.registerListener(mcm);
		//FunMidiEventRouter.FunListener fl = new FunMidiEventRouter.FunListener();
		//fmer.registerListener(fl);
		fmer.startPumpingMidiEvents();
	}

	public void setupRepoUpdateCallback() {
		RepoUpdateCallbackAdapter
				.registerCallback(new RepoUpdateCallbackAdapter.Callback() {
					public void repoUpdateCompleted() {
						getLogger()
								.info("c.h.b.oglweb.R50 activator got SPARQL-UPDATE callback");
						GruesomeTAProcessingFuncs.processPendingThingActions();
					}
				});
	}

	public void startServicePanel(final BundleContext context) {
		ServicesFrame.create(context);
	}

	static class AnimChanLifecycle extends
			AbstractLifecycleProvider<WantsThingAction, AnimOutTrigChan> {
		private List<ClassLoader> myResourceClassLoaders;

		public AnimChanLifecycle(String robotId,
				List<ClassLoader> resourceClassLoaders) {
			super(new DescriptorListBuilder()
					.dependency("animPlayer", AnimationPlayer.class)
					.with(AnimationPlayer.PROP_PLAYER_ID, robotId)
					.dependency("repoClient", RepoClient.class)
					.getDescriptors());
			if (myRegistrationProperties == null) {
				myRegistrationProperties = new Properties();
			}
			myRegistrationProperties.put("thingActionChannelAgentId", robotId);
			myResourceClassLoaders = resourceClassLoaders;
		}

		@Override
		protected AnimOutTrigChan create(Map<String, Object> dependencies) {
			RepoClient rc = (RepoClient) dependencies.get("repoClient");
			BehaviorConfigEmitter bce = new BehaviorConfigEmitter(rc,
					rc.makeIdentForQName(AnimFileSpecReader.animGraphQN()));
			AnimationPlayer player = (AnimationPlayer) dependencies
					.get("animPlayer");
			RobotAnimContext rac = new RobotAnimContext(new FreeIdent(
					"http://hrkind.com/robot#zenoR50"), bce);
			rac.setResourceClassLoaders(myResourceClassLoaders);
			rac.initConnForAnimPlayer(player);
			return rac.getTriggeringChannel();
		}

		@Override
		protected Class<WantsThingAction> getServiceClass() {
			return WantsThingAction.class;
		}

		@Override
		protected void handleChange(String name, Object dependency,
				Map<String, Object> availableDependencies) {
		}

	}

	/**
	 * Copied from Activator for Friendularity.Liftoff.
	 * 
	 * @param context
	 */
	public void initLifterWebappLifecycle(BundleContext context) {
		// Tell the lifter lifecycle to start, once its dependencies are satisfied
		LifterLifecycle lifecycle = new LifterLifecycle();
		OSGiComponent lifterComp = new OSGiComponent(context, lifecycle);
		lifterComp.start();
	}

	@Override
	protected CommonMediator getMediator() {
		return new CommonMediator(m_context);
	}

}
