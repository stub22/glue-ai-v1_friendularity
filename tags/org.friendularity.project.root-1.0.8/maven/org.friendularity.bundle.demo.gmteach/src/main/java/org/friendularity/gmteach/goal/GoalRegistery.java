package org.friendularity.gmteach.goal;

import java.util.Map;

import org.friendularity.bundle.demo.gmteach.GMTeachApp;

import ext.osgi.common.MacroBundleActivatorBase;

public class GoalRegistery extends GMTeachModule {

	final Map<String, GoalCommand> registeredGoals = MacroBundleActivatorBase.macroStartupSettings.actionCallbackMap;

	public GoalRegistery(GMTeachApp gmteach) {
		super(gmteach);
	}

	public void addGoal(String name, final Runnable r) {
		synchronized (registeredGoals) {
			registeredGoals.put(name, new GoalCommand(name) {
				@Override public void run() {
					r.run();
				}
			});
		}
	}

	@Override public void init(String[] args) {
	}

	@Override public void unload() {
	}

	@Override public String toString() {
		return "registeredGoals " + registeredGoals;
	}

}
