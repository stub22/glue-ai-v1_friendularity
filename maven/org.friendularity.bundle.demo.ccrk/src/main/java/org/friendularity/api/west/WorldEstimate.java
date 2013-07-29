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

import org.friendularity.api.goody.ShapeAnimator;
import org.friendularity.api.goody.VizShape;
import java.util.HashSet;
import java.util.Set;
import org.appdapter.core.name.Ident;
import org.appdapter.core.name.FreeIdent;
import com.jme3.math.Vector3f;
import com.jme3.math.ColorRGBA;
import org.cogchar.render.sys.registry.RenderRegistryClient;

/**
 * @author Stu B. <www.texpedient.com>
 */
public class WorldEstimate extends ThingEstimate {

	public static String ESTIM_NS = "http://friendularity.org/estimate#";
	public SelfEstimate mySelfEstim;
	// A person estimate is any hypothetical human, animal,  robot, or other "person" with agency for us to 
	// notice + interact with.  Presumably if we get "picked up" ourselves, it is probly by one of these agents.
	public Set<PersonEstimate> myPersonEstims = new HashSet<PersonEstimate>();
	// Anything else is stuff.
	public Set<StuffEstimate> myStuffEstims = new HashSet<StuffEstimate>();

	public WorldEstimate(Ident id) {
		super(id);
	}

	public void ensureSubpartsExist() {
		if (mySelfEstim == null) {
			Ident selfEstID = new FreeIdent(ESTIM_NS + "self_estim_88");
			mySelfEstim = new SelfEstimate(selfEstID);
			mySelfEstim.myPosVecExpr = "{5.0^Sin[$phaseAng], 6.0^(2*Cos[$phaseAng]), $phaseFrac}";
		}
		if ((myPersonEstims == null) || myPersonEstims.isEmpty()) {
			myPersonEstims = PersonEstimate.makeSampleSet();
		}

		if ((myStuffEstims == null) || myStuffEstims.isEmpty()) {
			myStuffEstims = StuffEstimate.makeSampleSet();
		}
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
			mathNeedsInit = false;
		}
		Object globs1 = mg.evalToIExpr("$elapsed:=$nowSec-$startSec; $cycles:=Floor[$elapsed/$cycleSec]");
		Object globs2 = mg.evalToIExpr("$phaseFrac:=$elapsed/$cycleSec-$cycles; $phaseAng:=2.0*Pi*$phaseFrac");

		super.updateFromMathSpace(mg);

		ensureSubpartsExist();

		mySelfEstim.updateFromMathSpace(mg);
		for (PersonEstimate pest : myPersonEstims) {
			pest.updateFromMathSpace(mg);
		}
		for (StuffEstimate sest : myStuffEstims) {
			sest.updateFromMathSpace(mg);
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
		for (StuffEstimate sest : myStuffEstims) {
			sest.renderAsSillyShape(viz);
		}
	}

	public static class SelfEstimate extends ThingEstimate {

		public SelfEstimate(Ident id) {
			super(id);
		}
	}

	public static class PersonEstimate extends ThingEstimate {

		private Integer myPersonIdx;

		public PersonEstimate(Ident id, Integer personIdx) {
			super(id);
			myPersonIdx = personIdx;
		}

		@Override
		public void updateFromMathSpace(MathGate mg) {
			mg.putVar("$personIdx", myPersonIdx);
			super.updateFromMathSpace(mg);
		}

		public static Set<PersonEstimate> makeSampleSet() {
			Set<PersonEstimate> sampleSet = new HashSet<PersonEstimate>();
			for (int i = 0; i < 10; i++) {
				Ident personID = new FreeIdent(ESTIM_NS + "person_estim_0" + i);
				PersonEstimate pest = new PersonEstimate(personID, i);
				double px = -10.0 - i * 2.0, py = 10.0 + i * 2.0, pz = 4.0;
				String baseVecExpr = "{" + px + ", " + py + ", " + pz + "}";
				pest.myPosVecExpr = "Sin[$phaseAng]*Sqrt[1.0 * $personIdx]*" + baseVecExpr;
				pest.myColorVecExpr = "{Sin[$phaseAng],Sin[3.0*$phaseAng], Sin[$phaseAng/2], $phaseFrac}";
				sampleSet.add(pest);
			}
			return sampleSet;
		}
	}

	public static class StuffEstimate extends ThingEstimate {

		static int jMax = 22;
		static int kMax = 18;
		private Integer myStuffIdx;

		public StuffEstimate(Ident id, Integer stuffIndex) {
			super(id);
			myStuffIdx = stuffIndex;
		}

		@Override
		public void updateFromMathSpace(MathGate mg) {
			mg.putVar("$stuffIdx", myStuffIdx);
			super.updateFromMathSpace(mg);
		}

		public static Set<StuffEstimate> makeSampleSet() {
			Set<StuffEstimate> sampleSet = new HashSet<StuffEstimate>();
			for (int j = 1; j <= jMax; j++) {
				// TODO - format the index number
				Ident stuffID = new FreeIdent(ESTIM_NS + "stuff_estim_0" + j);
				StuffEstimate sest = new StuffEstimate(stuffID, j);
				double px = 15.0 + j * 3.0, py = 10.0 + j * 3.0, pz = -12.0;
				// We assume that someone will post our stuffIdx before each eval of the following.
				// mg.putVar("$stuffIdx", new Integer(j));
				String baseVecExpr = "{" + px + ", " + py + ", " + pz + "}";
				sest.myPosVecExpr = "Sin[$phaseAng]*" + baseVecExpr;
				sest.myColorVecExpr = "{Sin[$stuffIdx * $phaseAng],Sin[3.0 * $phaseAng], Sin[$phaseAng/(2 * $stuffIdx)], $phaseFrac * $stuffIdx}";
				sampleSet.add(sest);
			}

			for (int k = 1; k <= kMax; k++) {
				int stuffIdx = k + jMax;
				Ident stuffID = new FreeIdent(ESTIM_NS + "stuff_estim_0" + stuffIdx);
				StuffEstimate sest = new StuffEstimate(stuffID, stuffIdx);
				double px = 15.0 + k * 3.0, py = 10.0 + k * 3.0, pz = -25.0;
				double kFrac = k / (double) kMax;
				double red = 0.4, green = 0.2, blue = 0.8, alpha = kFrac;
				sest.myColorVecExpr = "{" + red + ", " + green + ", " + blue + ", " + alpha + "}";
				double phaseOff = kFrac * 2 * Math.PI;
				sest.myPosVecExpr = "$phaseTot:=$phaseAng + " + phaseOff + "; {20.0*Cos[$phaseTot], $phaseTot * 5.0, 15.0*Sin[$phaseTot]}"; //  + baseVecExpr;
				sampleSet.add(sest);
			}
			return sampleSet;
		}

		@Override protected void attachSimpleVizObj(Visualizer viz) {
			if (myStuffIdx > jMax) {
				attachMonsterShape(viz);
			} else {
				super.attachSimpleVizObj(viz);
			}
		} 
		public void attachMonsterShape(Visualizer viz) {
			float initRadius = 5.0f;
			ColorRGBA initColor = ColorRGBA.Green;
			Vector3f basePos = new Vector3f(10.0f, 10.0f, 10.0f);
			myCachedVizObject = new VizShape(myIdent, basePos, initRadius, initColor);
			RenderRegistryClient rrc = viz.getRenderRegistryClient();
			ShapeAnimator sa = viz.getShapeAnimator();
			sa.attachChild_onRendThrd(rrc, myCachedVizObject);
		}
	}

	public static interface Consumer {

		public void setWorldEstimate(WorldEstimate worldEstim);
	}
}
