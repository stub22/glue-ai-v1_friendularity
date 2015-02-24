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

import org.appdapter.core.log.BasicDebugger;
import org.appdapter.core.name.Ident;
import org.cogchar.bind.symja.MathGate;

/**
 * @author Stu B. <www.texpedient.com>
 */
public abstract class ThingEstimate extends BasicDebugger {

	public Ident myIdent;

	public ThingEstimate(Ident id) {
		myIdent = id;
	}

	public Ident getIdent() {
		return myIdent;
	}

	public abstract void updateFromMathSpace(MathGate mg);

	public static abstract class CoordinateFrame {
		public Ident myIdent;
		public CoordinateFrame myParent;
	}

}
