package org.friendularity.gmteach.goal;

import org.appdapter.core.log.Debuggable;
import org.friendularity.bundle.demo.gmteach.GMTeachApp;

/**
 * @author Logicmoo <logicmoo@gmail.com>
 */

abstract public class GMTeachModule extends Debuggable {

	protected GMTeachApp myTeach;

	public GMTeachModule(GMTeachApp gmteach) {
		this.myTeach = gmteach;
		Runtime.getRuntime().addShutdownHook(new Thread("Shutdown hook for " + GMTeachModule.this) {
			@Override public void run() {
				try {
					unload();
				} catch (Throwable t0) {
					final Throwable t = t0;
					(new Thread("Error for " + GMTeachModule.this + " of " + t) {
						@Override public void run() {
							t.printStackTrace();
						}
					}).start();
				}
			}
		});
	}

	abstract public void init(String[] args);

	abstract public String toString();

	public void unload() {

	}
}
