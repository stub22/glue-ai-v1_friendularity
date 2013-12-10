/*
 *  Copyright 2012 by The Cogchar Project (www.cogchar.org).
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

package org.cogchar.app.puma.registry;

import org.appdapter.core.log.BasicDebugger;
import org.cogchar.app.puma.config.PumaContextMediator;

/**
 * @author Stu B. <www.texpedient.com>
 */

public class PumaGlobalPrebootInjector extends BasicDebugger {
	private static final PumaGlobalPrebootInjector theInjector = new PumaGlobalPrebootInjector();
	public static PumaGlobalPrebootInjector getTheInjector() {
		return theInjector;
	}
	
	private		PumaContextMediator		myMediator;
	
	public void setMediator(PumaContextMediator m, boolean overwriteFlag) {
		if ((myMediator != null) && (overwriteFlag == true)) {
			getLogger().warn("Overwriting existing mediator {} with new one {}", myMediator, m);
			myMediator = null;
		}
		if (myMediator == null) {
			myMediator = m;
		} else {
			getLogger().warn("Ignoring setMediator() request because overwriteFlag = false, keeping existing mediator {}");
		}
	}
	public PumaContextMediator getMediator() { 
		return myMediator;
	}
	
}
