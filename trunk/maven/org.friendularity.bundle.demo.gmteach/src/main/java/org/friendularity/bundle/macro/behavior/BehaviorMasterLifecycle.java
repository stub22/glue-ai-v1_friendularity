package org.friendularity.bundle.macro.behavior;

import java.util.Map;
import java.util.Properties;

import org.appdapter.help.repo.RepoClient;
import org.cogchar.outer.behav.demo.MasterDemo;
import org.cogchar.outer.behav.demo.TAGraphChanWiringDemo;
import org.cogchar.svc.behav.control.ActionCallbackMap;
import org.friendularity.bundle.demo.gmteach.LikeSuperActivator;
import org.jflux.impl.services.rk.lifecycle.AbstractLifecycleProvider;
import org.jflux.impl.services.rk.lifecycle.utils.DescriptorListBuilder;
import org.jflux.impl.services.rk.lifecycle.utils.SimpleLifecycle;
import org.jflux.impl.services.rk.osgi.lifecycle.OSGiComponent;
import org.osgi.framework.BundleContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * 
 * @author Ryan Biggs
 */
public class BehaviorMasterLifecycle extends AbstractLifecycleProvider<MasterDemo, MasterDemo> {
	private final static Logger theLogger = LoggerFactory.getLogger(BehaviorMasterLifecycle.class);

	private final static String queryEmitterId = "queryInterface";

	private BundleContext myContext;

	public BehaviorMasterLifecycle(BundleContext context) {
		super(new DescriptorListBuilder().dependency(queryEmitterId, RepoClient.class).getDescriptors());
		myContext = context;
	}

	@Override
	protected synchronized MasterDemo create(Map<String, Object> dependencies) {
		RepoClient repoClient = (RepoClient) dependencies.get(queryEmitterId);
		MasterDemo md = new MasterDemo();
		String adminID = "adminID";
		String behaviorID = "behaviorID";
		startTriggerPanel(myContext, behaviorID, adminID);
		//        startCallbackMaps(myContext, behaviorID, adminID);
		md.preLaunchSetup(myContext, LikeSuperActivator.ROBOT_CONNECTION_ENV_VAR_KEY);
		md.launchDemo(myContext, repoClient);
		try {
			TAGraphChanWiringDemo.loadAndRegisterSpecs(myContext, repoClient, "ccrt:taChan_sheet_77");
			//            TAGuardWiringDemo.loadAndRegisterSpecs(myContext, repoClient, "csi:behavStep_sheet_77", "csi:behavStep_sheet_78", "csi:behavStep_sheet_79", "csi:filters_sheet_77");
		} catch (Throwable t) {
			theLogger.warn("Failed to load TAGuards for Behavior Master", t);
		}
		SceneTriggerBinding.theMasterDemo = md;

		SceneTriggerBinding.launchSceneTriggerBindingLifecycle(myContext, behaviorID, adminID);
		ActionIconBinding.launchActionIconBindingLifecycle(myContext, behaviorID, adminID);

		return md;
	}

	@Override
	protected void handleChange(String serviceId, Object dependency, Map<String, Object> availableDependencies) {
		if (isSatisfied()) {
			myService = create(availableDependencies);
		} else {
			myService = null;
		}
	}

	@Override
	public Class<MasterDemo> getServiceClass() {
		return MasterDemo.class;
	}

	private void startTriggerPanel(final BundleContext context, final String behaviorPanelId, final String adminPanelId) {
		java.awt.EventQueue.invokeLater(new Runnable() {
			@Override
			public void run() {
				TriggerFrame tf = new TriggerFrame();
				tf.init(context, behaviorPanelId, adminPanelId);
				tf.setVisible(true);
			}
		});
	}

	private void startCallbackMaps(BundleContext context, String behaviorPanelId, String adminPanelId) {
		new OSGiComponent(context, new SimpleLifecycle(new ActionCallbackMapImpl(), ActionCallbackMap.class.getName(), props(adminPanelId))).start();
		new OSGiComponent(context, new SimpleLifecycle(new ActionCallbackMapImpl(), ActionCallbackMap.class.getName(), props(behaviorPanelId))).start();
	}

	private Properties props(String name) {
		Properties p = new Properties();
		p.put(TriggerPanel.PROP_TRIGGER_PANEL_ID, name);
		return p;
	}

}