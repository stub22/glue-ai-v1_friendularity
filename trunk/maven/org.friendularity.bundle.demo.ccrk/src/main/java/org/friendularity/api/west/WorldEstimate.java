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
import com.jme3.math.Vector3f;

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
	boolean mathNeedsInit = true;
	@Override
	public void updateFromMathSpace(MathGate mg) {
		long nowMsec = System.currentTimeMillis();
		double nowSec = nowMsec / 1000.0;
		mg.putVar("$nowSec", new Double(nowSec));
		if (mathNeedsInit) {
			mg.putVar("$startSec", new Double(nowSec));
			mg.putVar("$cycleSec", new Double(3.0));
			mathNeedsInit=false;
		}
		Object globs1 = mg.evalToIExpr("$elapsed:=$nowSec-$startSec; $cycles:=Floor[$elapsed/$cycleSec]");
		Object globs2 = mg.evalToIExpr("$phaseFrac:=$elapsed/$cycleSec-$cycles; $phaseAng:=2.0*Pi*$phaseFrac");
		super.updateFromMathSpace(mg);
		if (mySelfEstim == null) {
			Ident selfEstID = new FreeIdent(ESTIM_NS + "self_estim_88");
			mySelfEstim = new SelfEstimate(selfEstID);
			mySelfEstim.myPosVecExpr="{5.0^Sin[$phaseAng], 6.0^(2*Cos[$phaseAng]), $phaseFrac}";
			for (int i = 0; i < 10; i++) {
				Ident personID = new FreeIdent(ESTIM_NS + "person_estim_0" + i);
				PersonEstimate pest = new PersonEstimate(personID);
				double px = -10.0 - i * 2.0, py=10.0 + i * 2.0, pz = 4.0;
				String baseVecExpr = "{" + px + ", " + py + ", " + pz + "}";
				pest.myPosVecExpr="Sin[$phaseAng]*" + baseVecExpr;
				pest.myColorVecExpr = "{Sin[$phaseAng],Sin[3.0*$phaseAng], Sin[$phaseAng/2], $phaseFrac}";
				myPersonEstims.add(pest);
			}
		}
		mySelfEstim.updateFromMathSpace(mg);
		for (PersonEstimate pest : myPersonEstims) {
			pest.updateFromMathSpace(mg);
		}

	}

	@Override
	public void renderAsSillyShape(Visualizer viz) {
		if (mySelfEstim != null) {
			mySelfEstim.renderAsSillyShape(viz);
		}
		for (PersonEstimate pest : myPersonEstims) {
			pest.renderAsSillyShape(viz);
		}

		
	}

	public static class SelfEstimate extends ThingEstimate {

		public SelfEstimate(Ident id) {
			super(id);
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
