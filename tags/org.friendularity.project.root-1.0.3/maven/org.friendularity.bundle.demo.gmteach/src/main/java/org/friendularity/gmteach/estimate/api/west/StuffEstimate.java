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
package org.friendularity.gmteach.estimate.api.west;

import org.appdapter.core.name.Ident;
import org.cogchar.bind.symja.MathGate;

/**
 *
 * @author Stu B22 <stub22@appstract.com>
 */
public class StuffEstimate extends TrackingEstimate {
	private Integer myStuffIdx;

	public StuffEstimate(Ident id, Integer stuffIndex) {
		super(id);
		myStuffIdx = stuffIndex;
	}

	@Override public void updateFromMathSpace(MathGate mg) {
		mg.putVar("$stuffIdx", myStuffIdx);
		// super.updateFromMathSpace(mg);
	}
	
}
