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
		Runtime.getRuntime().addShutdownHook(new Thread() {

			@Override public void run() {
				unload();
			}
		});
	}

	abstract public void init(String[] args);

	public void unload() {

	}
}
