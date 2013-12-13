package org.friendularity.bundle.demo.gmteach;

import org.appdapter.gui.demo.DemoBrowser;
import org.friendularity.bundle.macro.behavior.BMDActivator;
import org.friendularity.bundle.macro.common.CommonActivator;
import org.friendularity.bundle.macro.common.CommonMediator;
import org.friendularity.bundle.macro.jvision.JVisionLikeActivator;
import org.friendularity.bundle.macro.liftapp.LiftAppActivator;
import org.friendularity.bundle.macro.tools.MoreToolsActivator;
import org.friendularity.bundle.macro.virtualworld.VirtualWorldActivator;
import org.osgi.framework.BundleContext;
import org.osgi.framework.BundleEvent;
import org.osgi.framework.BundleListener;
import org.osgi.framework.FrameworkEvent;
import org.osgi.framework.ServiceEvent;
import org.osgi.framework.ServiceListener;
import org.rwshop.swing.messaging.monitor.AvroTableDemoFrame;

import ext.osgi.common.MacroBundleActivatorBase;

// import org.appdapter.gui.demo.DemoBrowser;
// import org.cogchar.test.symcalc.ScriptEngineExperiment;
// import org.robokind.ui.swing.common.lifecycle.ServicesFrame;

public class GMTEACH_DemoActivator extends MacroBundleActivatorBase implements
		BundleListener, ServiceListener {

	@Override
	public void start(BundleContext bundleCtx) throws Exception {
		m_context = bundleCtx;
		// TODO Auto-generated method stub
		bundleCtx.addBundleListener(this);
		bundleCtx.addServiceListener(this);
		super.start(bundleCtx);
		bundleCtx.removeFrameworkListener(this);
		bundleCtx.addFrameworkListener(this);
		super.bundleBootPhase = BootPhaseConst.UNSTARTED;
		if (!MACRO_LAUNCHER) {
			getLogger()
					.error("org.friendularity.bundle.macrolauncher was not preceeded by org.friendularity.bundle.macro.common");
		}
		// take ownership as the primary bundle
		macroStartupSettings.makeStartupBundle(this);
		registerMacroLauncher(new BMDActivator());
		registerMacroLauncher(new JVisionLikeActivator());
		registerMacroLauncher(new LiftAppActivator());
		registerMacroLauncher(new MoreToolsActivator());
		registerMacroLauncher(new VirtualWorldActivator());
		// re-take ownership as the primary bundle
		macroStartupSettings.makeStartupBundle(this);
		setupDefaultConfigs(bundleCtx);
	}

	private void registerMacroLauncher(CommonActivator activator) {
		try {
			activator.start(m_context);
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

	}

	/**
	 * Receives notification of a general {@code FrameworkEvent} object.
	 * 
	 * @param event
	 *            The {@code FrameworkEvent} object.
	 */
	public void frameworkEvent(FrameworkEvent event) {
		Throwable throwable = event.getThrowable();
		if (throwable != null)
			throwable.printStackTrace();
		System.err.println("framework event " + event);
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
		putSetting("connectMidiOut", false);
		putSetting("connectSwingDebugGUI", false);
		putSetting("monitorLifecycles", startAll);
		putSetting("VirtualWorld", startAll);
		putSetting("Behavior", startAll);
		putSetting("ServiceMonitor", startAll);
		putSetting("AvroMessages", startAll);
		putSetting("LiftApp", startAll);
		putSetting("DeicticVisualizer", true);
	}

	private void putSetting(String key, boolean value) {
		macroStartupSettings.putSetting(key, value);

	}

	private void setupDefaultConfigs(final BundleContext context) {
		setDefaultSettings(true);
		org.friendularity.bundle.macro.tools.MoreToolsActivator.LAUNCH_MYSELF = false;

		// Have to set this bool early in start() here
		org.friendularity.bundle.jvision.JVisionBundleActivator.LAUNCH_MYSELF = false;// flagTrue("connectJVision");
		//putSetting("VirtualWorld", false);
		putSetting("LifterPuma", false);
		putSetting("worldEstimate", false);
		putSetting("scenelifecycledemotest", false);

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

	@Override
	public void registerServices(BundleContext context0) {
		addMacroService("startQPIDMonitor", new Runnable() {
			@Override
			public void run() {
				startQPIDMonitor(m_context);
			}
		});
	}

	public void launchApplication(BundleContext context) throws Exception {
		if (this.bundleBootPhase < BootPhaseConst.LAUNCHING) {
			this.bundleBootPhase = BootPhaseConst.LAUNCHING;
			macroStartupSettings.makeStartupBundle(this);
			if (!isLauncherBundle()) {
				showDemoApp();
			}
			runDemoApp();
			this.bundleBootPhase = BootPhaseConst.LAUNCHING_COMPLETE;
		}
	}

	public void runDemoApp() {
		new Thread(new Runnable() {

			@Override
			public void run() {
				try {
					Thread.sleep(10000);
				} catch (InterruptedException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
				macroStartupSettings.launchPhases();
			}
		}).start();
	}

	private void showDemoApp() {

		DemoBrowser.appName = "MacroLauncher";
		DemoBrowser.addRepoLoaderMenu();
		DemoBrowser.showObject(macroStartupSettings);
		org.appdapter.gui.browse.Utility.setSingletonValue(BundleContext.class,
				m_context);
		DemoBrowser.show();
	}

	protected CommonMediator getMediator() {
		return new CommonMediator(m_context);
	}

}
