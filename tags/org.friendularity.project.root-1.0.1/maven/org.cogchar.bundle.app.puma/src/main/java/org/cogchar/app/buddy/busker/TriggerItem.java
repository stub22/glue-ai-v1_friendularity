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
package org.cogchar.app.buddy.busker;
import org.appdapter.core.log.BasicDebugger;
import org.cogchar.platform.trigger.CogcharScreenBox;
import org.cogchar.platform.trigger.CogcharActionTrigger;


/**
 * @author Stu B. <www.texpedient.com>
 * Base impl class for CogcharTriggers, providing the inherited identifier+name features of KnownComponentImpl,
 * and providing a convenience logging method.
 */
public abstract class TriggerItem extends org.appdapter.bind.rdf.jena.assembly.KnownComponentImpl implements CogcharActionTrigger {
	protected void logFiring(CogcharScreenBox targetBox, Object extraInfoObj, Object extraDebugObj) {
		getLogger().info("Trigger[class={}, id={}] firing on box [class={}, id={}], extraInfo=[{}]", this.getClass(), 
					this.getIdent(), targetBox.getClass().getName(), targetBox.getIdent(), extraInfoObj);	
		getLogger().debug("Full debug dump of trigger [{}]\nand box:\n[{}]\nextraDebug=[{}]", this, targetBox, extraDebugObj);
	}
	protected void logFiring(CogcharScreenBox targetBox) {
		logFiring(targetBox, null, null);
	}	
	
}
