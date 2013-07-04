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
import org.appdapter.core.name.FreeIdent;

/**
 * @author Stu B. <www.texpedient.com>
 */
public class WorldEstimate extends ThingEstimate {

	public	static String ESTIM_NS = "http://friendularity.org/estimate#";
	
	public SelfEstimate mySelfEstim;
	public Set<PersonEstimate> myPersonEstims = new HashSet<PersonEstimate>();
	public Set<StuffEstimate> myStuffEstims = new HashSet<StuffEstimate>();

	public WorldEstimate(Ident id) {
		super(id);
	}

	@Override
	public void updateFromMathSpace(MathSpace ms) {
		if (mySelfEstim == null) {
			Ident selfEstID = new FreeIdent(ESTIM_NS + "self_estim_88");
			mySelfEstim = new SelfEstimate(selfEstID);
		}
		mySelfEstim.updateFromMathSpace(ms);


	}

	@Override
	public void renderAsSillyShape(Visualizer viz) {
		if (mySelfEstim != null) {
			mySelfEstim.renderAsSillyShape(viz);
		}
	}

	public static class SelfEstimate extends ThingEstimate {

		public SelfEstimate(Ident id) {
			super(id);
		}

		@Override
		public void updateFromMathSpace(MathSpace ms) {
			// First goal is to Get X, Y, Z + Rot-X, Rot-Y, Rot-Z
			// Formal but costly approach:  Serialize my previous data into MathSpace, calc update, serialize back out.
			// Alternative is to assume a symbiosis:  MathSpace knows what's up!  In this case, we at least serialize
			// in our own ident, and look up state on that basis.  
		}

		@Override
		public void renderAsSillyShape(Visualizer viz) {
			// Move the character's location + orientation to the values from MathSpace estimate.
			super.renderAsSillyShape(viz);
		}
	}

	public static class PersonEstimate extends ThingEstimate {

		public PersonEstimate(Ident id) {
			super(id);
		}
	}

	public static class StuffEstimate extends ThingEstimate {

		public StuffEstimate(Ident id) {
			super(id);
		}
	}

	public static interface Consumer {

		public void setWorldEstimate(WorldEstimate worldEstim);
	}
}
