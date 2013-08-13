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
import org.friendularity.vworld.VisionTextureMapper;
/**
 * @author Stu B. <www.texpedient.com>
 */
public class WorldEstimate extends ThingEstimate {

	public static String ESTIM_NS = "http://friendularity.org/estimate#";
	public SelfEstimate					mySelfEstim;
	// A person estimate is any hypothetical human, animal,  robot, or other "person" with agency for us to 
	// notice + interact with.  Presumably if we get "picked up" ourselves, it is probly by one of these agents.
	public Set<PersonEstimate>			myPersonEstims = new HashSet<PersonEstimate>();
	// Anything else is stuff.
	public Set<StuffEstimate>			myStuffEstims = new HashSet<StuffEstimate>();

	private VisionTextureMapper			myVTM;
	
	public WorldEstimate(Ident id) {
		super(id);
	}

	public void ensureSubpartsExist() {
		if (mySelfEstim == null) {
			Ident selfEstID = new FreeIdent(ESTIM_NS + "self_estim_88");
			mySelfEstim = new SelfEstimate(selfEstID);
			String selfPosVecExpr = "{5.0^Sin[$phaseAng], 6.0^(2*Cos[$phaseAng]), $phaseFrac}";
			mySelfEstim.setPosMathExpr(selfPosVecExpr);
		}
		if ((myPersonEstims == null) || myPersonEstims.isEmpty()) {
			myPersonEstims = EstimateLib.makeFuinPersonEstims();
		}

		if ((myStuffEstims == null) || myStuffEstims.isEmpty()) {
			myStuffEstims = EstimateLib.makeFunStuffEstims(12, 8);
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
		// Running these expressions updates some variables within the mathGate, used by demonstration oscillators.
		Object globs1 = mg.evalToIExpr("$elapsed:=$nowSec-$startSec; $cycles:=Floor[$elapsed/$cycleSec]");
		Object globs2 = mg.evalToIExpr("$phaseFrac:=$elapsed/$cycleSec-$cycles; $phaseAng:=2.0*Pi*$phaseFrac");

		// This reads a new Vector3f and Color4f object every time, which is expensive, and possibly leaky in
		// some non-obvious way?
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
	public void renderAsSillyShape(Visualizer viz, float timePerFrame) {
		if (mySelfEstim != null) {
			mySelfEstim.renderAsSillyShape(viz, timePerFrame);
		}
		for (PersonEstimate pest : myPersonEstims) {
			pest.renderAsSillyShape(viz, timePerFrame);
		}
		for (StuffEstimate sest : myStuffEstims) {
			sest.renderAsSillyShape(viz, timePerFrame);
		}
		if (myVTM == null) {
			myVTM = new VisionTextureMapper();
			RenderRegistryClient rrc = viz.getRenderRegistryClient();
			myVTM.setup(rrc);
		}
		myVTM.simpleUpdate(timePerFrame);
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

		@Override public void updateFromMathSpace(MathGate mg) {
			mg.putVar("$personIdx", myPersonIdx);
			super.updateFromMathSpace(mg);
		}
	}

	public static class StuffEstimate extends ThingEstimate {
		public enum Kind {
			REGULAR,
			MONSTER
		}
		private Integer myStuffIdx;
		private Kind myKind = Kind.REGULAR;

		public StuffEstimate(Ident id, Integer stuffIndex, Kind shapeKind) {
			super(id);
			myStuffIdx = stuffIndex;
			myKind = shapeKind;
		}
		@Override public void updateFromMathSpace(MathGate mg) {
			mg.putVar("$stuffIdx", myStuffIdx);
			super.updateFromMathSpace(mg);
		}
		@Override protected void attachSimpleVizObj(Visualizer viz) {
			if (myKind == Kind.MONSTER) { 
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
