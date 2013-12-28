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

import org.appdapter.core.name.Ident;
import org.cogchar.bind.symja.MathGate;


import org.friendularity.struct.*;

/**
 * @author Stu B. <www.texpedient.com>
 */

public class TrackingEstimate extends ThingEstimate {
	private		RingBuf<Struct<String, ArrayOfDoubles>>		mySnaps;

	public TrackingEstimate(Ident id) {
		super(id);
	}
	@Override public void updateFromMathSpace(MathGate mg) {
		throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
	}
	
}
