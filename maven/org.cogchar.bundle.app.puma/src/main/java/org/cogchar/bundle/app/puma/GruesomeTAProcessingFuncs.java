/*
 *  Copyright 2013 by The Cogchar Project (www.cogchar.org).
 * 
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 * 
 *       http://www.apache.org/licenses/LICENSE-2.0
 * 
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package org.cogchar.bundle.app.puma;

import org.appdapter.core.name.FreeIdent;
import org.appdapter.core.name.Ident;
import org.cogchar.impl.thing.basic.BasicThingActionRouter;
import static org.cogchar.name.entity.EntityRoleCN.RKRT_NS_PREFIX;

/**
 * @author Stu B. <www.texpedient.com>
 */

public class GruesomeTAProcessingFuncs {
	private static BasicThingActionRouter	theRouter;
	public static BasicThingActionRouter	getActionRouter() {
		if (theRouter == null) {
            String localName = "theRouter";
            Ident agentID = new FreeIdent(RKRT_NS_PREFIX + localName, localName);
            theRouter = new BasicThingActionRouter(0L, agentID);
		}
		return theRouter;
	}
	
	@Deprecated public static void registerActionConsumers() { 
		PumaAppUtils.GreedyHandleSet srec = new PumaAppUtils.GreedyHandleSet();
		// The VWorld does its own registration in a separate ballet.
		// Here we are just handling the reg for Web + Behavior.

		BasicThingActionRouter router = getActionRouter();
		srec.pumaWebMapper.registerActionConsumers(router, srec.rc, srec.gce);		
	}
	/* This is called in two separate cases:
	 *		1) In the repoUpdateCompleted() callback of a top-level application, e.g.
	 *			a) o.f.b.demo.liftoff.Activator
	 *			b) c.h.b.oglweb.R50.Activator
	 *		2) On a call to  PumaAppContext.resetMainConfigAndCheckThingActions
	 *			which is queued indirectly from   PumaContextCommandBox.processUpdateRequestAsync
	 *			which is a crude, old form of GUI wiring to be replaced.
	 *	
	 */
	@Deprecated public static void processPendingThingActions() {
		PumaAppUtils.GreedyHandleSet srec = new PumaAppUtils.GreedyHandleSet();	
		BasicThingActionRouter router = getActionRouter();
		// Only known call to this 1-arg method form.
		router.consumeAllActions(srec.rc);
	}
}
