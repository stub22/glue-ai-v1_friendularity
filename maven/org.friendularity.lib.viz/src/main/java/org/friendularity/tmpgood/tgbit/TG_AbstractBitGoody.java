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

package org.friendularity.tmpgood.tgbit;

import org.appdapter.core.name.Ident;
import org.cogchar.name.goody.GoodyNames;
import org.cogchar.render.app.entity.GoodyActionExtractor;
import org.cogchar.render.goody.basic.BasicGoodyCtx;
import org.cogchar.render.goody.basic.BasicGoodyEntity;

/**
 * An abstract class containing elements common to Goody "bit" objects
 * 
 * @author Ryan Biggs
 */


public abstract class TG_AbstractBitGoody extends BasicGoodyEntity {
	
	protected boolean state = false;
	
	protected TG_AbstractBitGoody(BasicGoodyCtx bgc, Ident uri) {
		super(bgc, uri);
	}
	
	public void setZeroState(QueueingStyle qStyle) {
		setState(false, qStyle);
	}
	
	public void setOneState(QueueingStyle qStyle) {
		setState(true, qStyle);
	}
	
	public void toggleState(QueueingStyle qStyle) {
		setState(!state, qStyle);
	}
	
	public abstract void setState(boolean boxState, QueueingStyle qStyle);
	
	@Override public void applyAction(GoodyActionExtractor ga, QueueingStyle qStyle) {
		super.applyAction(ga, qStyle); // Applies "standard" set and move actions
		// Now we act on anything else that won't be handled by BasicGoodyImpl but which has valid non-null parameters
		switch (ga.getKind()) {
			case SET : {
				String stateString = ga.getSpecialString(GoodyNames.BOOLEAN_STATE);
				if (stateString != null) {
					try {
						setState(Boolean.valueOf(stateString), qStyle);
					} catch (Exception e) { // May not need try/catch after BasicTypedValueMap implementation is complete
						getLogger().error("Error setting box state to state string {}", stateString, e);
					}
				}
				break;
			}
		}
	}
}
