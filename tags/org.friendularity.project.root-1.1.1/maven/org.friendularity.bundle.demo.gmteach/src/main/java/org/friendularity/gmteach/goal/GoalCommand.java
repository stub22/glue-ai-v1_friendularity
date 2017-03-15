package org.friendularity.gmteach.goal;

import org.appdapter.core.log.Debuggable;
import org.appdapter.core.name.FreeIdent;
import org.appdapter.core.name.Ident;

abstract public class GoalCommand extends Debuggable implements Runnable {

	public static String GOAL_NS = "http://friendularity.org/goal#";

	public Ident myIdent;

	public GoalCommand(String id) {
		myIdent = new FreeIdent(GOAL_NS + id);
	}

	public Ident getIdent() {
		return myIdent;
	}
}
