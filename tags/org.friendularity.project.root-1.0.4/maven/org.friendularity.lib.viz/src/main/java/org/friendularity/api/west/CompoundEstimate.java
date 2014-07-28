/*
 *  Copyright 2013 by The Friendularity Project (www.friendularity.org).
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

package org.friendularity.api.west;

import java.util.HashSet;
import java.util.Set;
import org.appdapter.core.name.Ident;
import org.cogchar.bind.symja.MathGate;

/**
 * @author Stu B. <www.texpedient.com>
 */

public abstract class CompoundEstimate extends ThingEstimate {
	public CompoundEstimate(Ident id) {
		super(id);
	}
	protected abstract void ensureSubpartsExist();
	
	public Set<ThingEstimate> getSubEstimates() { 
		return new HashSet<ThingEstimate>();
	}
	

	@Override public void updateFromMathSpace(MathGate mg) {

		Set<ThingEstimate> subEstims = getSubEstimates();
		for (ThingEstimate subEstim : subEstims) {
			subEstim.updateFromMathSpace(mg);
		}
		
		// logInfo("Estimate " + myIdent + " of type " + getClass() + " read position: " + myCachedPosVec3f);
		// First goal is to Get X, Y, Z + Rot-X, Rot-Y, Rot-Z
		// Formal but costly approach:  Serialize my previous data into MathSpace, calc update, serialize back out.
		// Alternative is to assume a symbiosis:  MathSpace knows what's up!  In this case, we at least serialize
		// in our own ident, and look up state on that basis.  
			
		// Another way is to define a custom *function* as the data provider. In this way we aren't required
		// to update symbol tables on each pass, only the data that our custom function accesses.
	}
}
