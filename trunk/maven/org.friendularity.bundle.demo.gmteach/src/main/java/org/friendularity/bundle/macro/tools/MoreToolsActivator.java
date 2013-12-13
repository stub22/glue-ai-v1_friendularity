package org.friendularity.bundle.macro.tools;

import static org.friendularity.bundle.macro.tools.R50ConfigUtils.JOINT_GROUP_XML_CONFIG_PATH;
import static org.friendularity.bundle.macro.tools.R50ConfigUtils.ROBOT_XML_CONFIG_PATH;
import static org.friendularity.bundle.macro.tools.R50ConfigUtils.VISEME_JSON_CONFIG_PATH;

import java.util.List;
import java.util.Map;
import java.util.Properties;

import javax.swing.UIManager;

import org.appdapter.core.name.FreeIdent;
import org.appdapter.gui.demo.DemoBrowser;
import org.appdapter.help.repo.RepoClient;
import org.cogchar.api.thing.WantsThingAction;
import org.cogchar.app.puma.web.LifterLifecycle;
import org.cogchar.bind.rk.behavior.SceneLifecycleDemo;
import org.cogchar.bind.rk.robot.client.AnimOutTrigChan;
import org.cogchar.bind.rk.robot.client.RobotAnimContext;
import org.cogchar.blob.emit.BehaviorConfigEmitter;
import org.cogchar.bundle.app.puma.GruesomeTAProcessingFuncs;
import org.cogchar.bundle.app.puma.PumaAppUtils;
import org.cogchar.impl.channel.AnimFileSpecReader;
import org.cogchar.joswrap.RepoUpdateCallbackAdapter;
import org.cogchar.svc.behav.control.BehaviorControlServiceManager;
import org.friendularity.bundle.macro.common.CommonActivator;
import org.friendularity.bundle.macro.common.CommonMediator;
import org.jflux.api.core.config.Configuration;
import org.jflux.impl.registry.OSGiRegistry;
import org.jflux.impl.services.rk.lifecycle.AbstractLifecycleProvider;
import org.jflux.impl.services.rk.lifecycle.utils.DescriptorListBuilder;
import org.jflux.impl.services.rk.osgi.lifecycle.OSGiComponent;
import org.osgi.framework.BundleContext;
import org.robokind.api.animation.player.AnimationPlayer;
import org.robokind.api.motion.Robot;
import org.rwshop.swing.common.lifecycle.ServicesFrame;
import org.rwshop.swing.messaging.monitor.AvroTableDemoFrame;
// import org.appdapter.gui.demo.DemoBrowser;
// import org.cogchar.test.symcalc.ScriptEngineExperiment;
// import org.robokind.ui.swing.common.lifecycle.ServicesFrame;

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
public class MoreToolsActivator extends CommonActivator {

	public void startQPIDMonitor(final BundleContext context) throws Exception {
		getLogger().info("AQServiceSwingUI Activation Begin.");

		try {
			UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
		} catch (Exception ex) {
			java.util.logging.Logger.getLogger(
					AvroTableDemoFrame.class.getName()).log(
					java.util.logging.Level.SEVERE, null, ex);
		}

		java.awt.EventQueue.invokeLater(new Runnable() {

			public void run() {
				AvroTableDemoFrame frame = new AvroTableDemoFrame();
				frame.start(context);
				frame.setVisible(true);
			}
		});
		getLogger().info("AQServiceSwingUI Activation Complete.");
	}

	protected void startLifecycleMonitorGuiWindow(final BundleContext context) {

		ServicesFrame.create(context);

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
		DemoBrowser.show();
	}

	public void registerServices(BundleContext context0) {

		addMacroService("monitorLifecycles", new Runnable() {
			@Override
			public void run() {
				startLifecycleMonitorGuiWindow(m_context);
			}
		});

		addMacroService("LifterPuma", new Runnable() {
			public void run() {
				configPuma();
				bootPuma();
				addMacroService("checkAnimFiles", new Runnable() {
					@Override
					public void run() {
						PumaAppUtils.checkAnimationFiles(null);
					}
				});
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

		addMacroService("startR50", new Runnable() {
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
					Class.forName(
							"org.cogchar.test.symcalc.ScriptEngineExperiment")
							.getMethod("main", String[].class)
							.invoke(null, null);
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

		addMacroService("ServicePanel", new Runnable() {
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

		addMacroService("AnimChanLifecycle", new Runnable() {
			@Override
			public void run() {
				new OSGiComponent(m_context, new AnimChanLifecycle("myRobot",
						getMediator().getFileResClassLoaders())).start();
			}
		});

		addMacroService("startSillyMotionComputersDemoForVWorldOnly",
				new Runnable() {
					@Override
					public void run() {
						Robot.Id optRobotID_elseAllRobots = null;
						PumaAppUtils
								.startSillyMotionComputersDemoForVWorldOnly(
										m_context, optRobotID_elseAllRobots);
					}
				});
	}

	private void setupRepoUpdateCallback() {
		RepoUpdateCallbackAdapter
				.registerCallback(new RepoUpdateCallbackAdapter.Callback() {
					public void repoUpdateCompleted() {
						getLogger()
								.info("c.h.b.oglweb.R50 activator got SPARQL-UPDATE callback");
						GruesomeTAProcessingFuncs.processPendingThingActions();
					}
				});
	}

	private void startR50(BundleContext context) {
		Configuration<String> conf = R50ConfigUtils.getDefaultR50Config();
		conf.getPropertySetter(ROBOT_XML_CONFIG_PATH).handleEvent(
				"/home/fit/robokind/resources/robot.xml");
		conf.getPropertySetter(JOINT_GROUP_XML_CONFIG_PATH).handleEvent(
				"/home/fit/robokind/resources/jointgroup.xml");
		conf.getPropertySetter(VISEME_JSON_CONFIG_PATH).handleEvent(
				"/home/fit/robokind/resources/VisemeConf.json");
		R50ConfigUtils.startR50(context, conf);
		//        startServicePanel(context);
	}

	private void startServicePanel(final BundleContext context) {
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
