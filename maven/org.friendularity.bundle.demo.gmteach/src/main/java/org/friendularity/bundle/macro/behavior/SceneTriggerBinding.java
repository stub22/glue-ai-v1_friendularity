/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.friendularity.bundle.macro.behavior;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.Map;

import org.appdapter.core.log.BasicDebugger;
import org.cogchar.api.scene.Scene;
import org.cogchar.impl.scene.BScene;
import org.cogchar.outer.behav.demo.MasterDemo;
import org.cogchar.outer.behav.impl.OSGiTheater;
import org.cogchar.svc.behav.control.ActionCallbackMap;
import org.jflux.impl.services.rk.lifecycle.AbstractLifecycleProvider;
import org.jflux.impl.services.rk.lifecycle.utils.DescriptorListBuilder;
import org.jflux.impl.services.rk.osgi.ServiceClassListener;
import org.jflux.impl.services.rk.osgi.lifecycle.OSGiComponent;
import org.osgi.framework.BundleContext;

/**
 *
 * @author matt
 * @author stub
 * @author dmiles
 * @author eadsjr
 */
public class SceneTriggerBinding extends BasicDebugger {

	private OSGiTheater myOSGiTheater;
	private ActionCallbackMap myBehaviorActionMap;
	private ActionCallbackMap myAdminActionMap;
	private SceneTriggerBinding.SceneListener mySceneListener;
	public static MasterDemo theMasterDemo;

	public SceneTriggerBinding(OSGiTheater osgiTheater, ActionCallbackMap behaviorMap, ActionCallbackMap adminMap) {
		if (osgiTheater == null || behaviorMap == null || adminMap == null) {
			throw new NullPointerException();
		}
		myOSGiTheater = osgiTheater;
		myBehaviorActionMap = behaviorMap;
		myAdminActionMap = adminMap;
		mySceneListener = new SceneTriggerBinding.SceneListener();
		Scene[] scenes = myOSGiTheater.getScenes().toArray(new Scene[0]);
		myOSGiTheater.addPropertyChangeListener(mySceneListener);
		if (scenes == null || scenes.length == 0) {
			getLogger().error("No Scenes here!");
			myOSGiTheater.start();
			scenes = myOSGiTheater.getScenes().toArray(new Scene[0]);
		}
		for (Scene scene : scenes) {
			mySceneListener.propertyChange(new PropertyChangeEvent(this, ServiceClassListener.PROP_SERVICE_ADDED, null, scene));
		}

		//TODO: extract this data to a source
		myAdminActionMap.putActionCallback("ADMIN-Reload_Content_and_Reset", new SceneTriggerBinding.Lstn_ReloadSceneContent());
		myAdminActionMap.putActionCallback("ADMIN-Stop_Behavior", new SceneTriggerBinding.Lstn_RestartTheater());
		myAdminActionMap.putActionCallback("ADMIN-Show_GUI", new SceneTriggerBinding.Lstn_ShowGUI());
	}

	class SceneListener implements PropertyChangeListener {

		@Override
		public void propertyChange(PropertyChangeEvent evt) {
			if (evt == null) {
				return;
			}
			BScene scene = evt.getNewValue() instanceof BScene ? (BScene) evt.getNewValue() : null;
			if (scene == null) {
				return;
			}
			// Another approach would be to use the "trigger(Name)" field.
			String sceneName = scene.mySceneSpec().getIdent().getLocalName();
			if (ServiceClassListener.PROP_SERVICE_ADDED.equals(evt.getPropertyName())) {
				getLogger().info("adding button-action-callback for sceneName {}", sceneName);
				myBehaviorActionMap.putActionCallback(sceneName, new SceneTriggerBinding.Lstn_SceneLaunch(scene));
			} else if (ServiceClassListener.PROP_SERVICE_REMOVED.equals(evt.getPropertyName())) {
				getLogger().info("removing button-action-callback for sceneName {}", sceneName);
				myBehaviorActionMap.removeActionCallback(sceneName);
			}
		}
	}

	private class Lstn_SceneLaunch implements ActionListener {

		private BScene myScene;

		public Lstn_SceneLaunch(BScene scene) {
			myScene = scene;
		}

		@Override
		public void actionPerformed(ActionEvent e) {
			if (theMasterDemo != null) {
				boolean cancelExisitingOutJobs = true;
				theMasterDemo.myTheaterWiringDemo.playSceneCleanly(myOSGiTheater, myScene, cancelExisitingOutJobs);
			} else {
				getLogger().warn("Got SceneLaunch.actionPerf(), but cannot find theSceneWiringDemo to play through!");
			}
		}
	}

	private class Lstn_RestartTheater implements ActionListener {

		@Override
		public void actionPerformed(ActionEvent e) {
			// This can take up to a few seconds, depending on log level.  
			if (theMasterDemo != null) {
				boolean cancelExisitingOutJobs = true;
				theMasterDemo.myTheaterWiringDemo.stopAndRestartTheater(myOSGiTheater, cancelExisitingOutJobs);
			} else {
				getLogger().warn("Got RestartTheater.actionPerf(), but cannot find theSceneWiringDemo to RestartTheater!");
			}
		}
	}

	private class Lstn_ShowGUI implements ActionListener {

		@Override public void actionPerformed(ActionEvent e) {
			showAdminGUI(SceneTriggerBinding.this, true, true);

		}
	}

	private class Lstn_ReloadSceneContent implements ActionListener {

		@Override
		public void actionPerformed(ActionEvent e) {
			// This can take a while, since we are reloading from some resource, sometimes over network.
			if (theMasterDemo != null) {
				boolean cancelExisitingOutJobs = true;
				theMasterDemo.reloadScenesAndRestartTheater(myOSGiTheater, cancelExisitingOutJobs);
			} else {
				getLogger().warn("Got ReloadScenesAction.actionPerf(), but cannot find theSceneWiringDemo to reload!");
			}
		}
	}

	public static class SceneTriggerBindingLifecycle extends AbstractLifecycleProvider<SceneTriggerBinding, SceneTriggerBinding> {

		private final static String OSGI_THEATER_DEP_KEY = "osgiTheater";
		private final static String BEHAVIOR_ACTION_MAP_DEP_KEY = "behaviorActionMap";
		private final static String ADMIN_ACTION_MAP_DEP_KEY = "adminActionMap";

		public SceneTriggerBindingLifecycle(String behaviorPanelId, String adminPanelId) {
			super(new DescriptorListBuilder()
                    .dependency(OSGI_THEATER_DEP_KEY, OSGiTheater.class)
                    .dependency(BEHAVIOR_ACTION_MAP_DEP_KEY, ActionCallbackMap.class)
                            .with(TriggerPanel.PROP_TRIGGER_PANEL_ID, behaviorPanelId)
                    .dependency(ADMIN_ACTION_MAP_DEP_KEY, ActionCallbackMap.class)
                            .with(TriggerPanel.PROP_TRIGGER_PANEL_ID, adminPanelId)
					.getDescriptors());
		}

		@Override
		protected SceneTriggerBinding create(Map<String, Object> dependencies) {
			return new SceneTriggerBinding((OSGiTheater) dependencies.get(OSGI_THEATER_DEP_KEY), (ActionCallbackMap) dependencies.get(BEHAVIOR_ACTION_MAP_DEP_KEY),
					(ActionCallbackMap) dependencies.get(ADMIN_ACTION_MAP_DEP_KEY));
		}

		@Override
		protected void handleChange(String name, Object dependency, Map<String, Object> availableDependencies) {
		}

		@Override
		protected Class<SceneTriggerBinding> getServiceClass() {
			return SceneTriggerBinding.class;
		}
	}

	public static void launchSceneTriggerBindingLifecycle(BundleContext context, String behaviorPanelId, String adminPanelId) {
		new OSGiComponent(context, new SceneTriggerBinding.SceneTriggerBindingLifecycle(behaviorPanelId, adminPanelId)).start();
    }
    
	public static void showAdminGUI(Object any, boolean showASAP, boolean loadChildrenTo) {
//		DemoBrowser.showObject(null, any, showASAP, loadChildrenTo);
	}
}
