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

// import org.cogchar.test.symcalc.ParamChunk;
import org.cogchar.bind.symja.MathGate;
import org.cogchar.render.goody.dynamic.VizShapeGroup;
import org.cogchar.render.goody.dynamic.VizShape;
import java.util.HashSet;
import java.util.Set;
import org.appdapter.core.name.Ident;
import org.appdapter.core.name.FreeIdent;

import org.cogchar.render.sys.registry.RenderRegistryClient;
import org.friendularity.vworld.VisionTextureMapper;
/**
 * @author Stu B. <www.texpedient.com>
 */
public class WorldEstimate extends CompoundEstimate {

	public static String ESTIM_NS = "http://friendularity.org/estimate#";
	public SelfEstimate					mySelfEstim;
	// A person estimate is any hypothetical human, animal,  robot, or other "person" with agency for us to 
	// notice + interact with.  Presumably if we get "picked up" ourselves, it is probly by one of these agents.
	public Set<PersonEstimate>			myPersonEstims = new HashSet<PersonEstimate>();
	// Anything else is stuff.
	public Set<StuffEstimate>			myStuffEstims = new HashSet<StuffEstimate>();

	boolean mathNeedsInit = true;

	public double mult_A = 1.0;
	
//	public ParamChunk myNumChunk = new ParamChunk.Number(), myTxtChunk = new ParamChunk.Text();
	
	public WorldEstimate(Ident id) {
		super(id);
	}

	@Override protected void ensureSubpartsExist() {
		if (mySelfEstim == null) {
			Ident selfEstID = new FreeIdent(ESTIM_NS + "self_estim_88");
			mySelfEstim = new SelfEstimate(selfEstID);
			// mySelfEstim.setPosMathExpr(selfPosVecExpr);
		}
		if ((myPersonEstims == null) || myPersonEstims.isEmpty()) {
			myPersonEstims = EstimateLib.makeFunPersonEstims();
		}

		if ((myStuffEstims == null) || myStuffEstims.isEmpty()) {
			myStuffEstims = EstimateLib.makeFunStuffEstims(12, 8);
		}
	}

	
	@Override public void updateFromMathSpace(MathGate mg) {
		long nowMsec = System.currentTimeMillis();
		double nowSec = nowMsec / 1000.0;
		mg.putVar("$nowSec", new Double(nowSec));
		mg.putVar("$multA", new Double(mult_A));
		
		if (mathNeedsInit) {
			mg.putVar("$startSec", new Double(nowSec));
			mg.putVar("$cycleSec", new Double(3.0));
			mathNeedsInit = false;
		}
		// Running these expressions updates some variables within the mathGate, used by demonstration oscillators.
		Object globs1 = mg.parseAndEvalExprToIExpr("$elapsed:=$nowSec-$startSec; $cycles:=Floor[$elapsed/$cycleSec]");
		Object globs2 = mg.parseAndEvalExprToIExpr("$phaseFrac:=$elapsed/$cycleSec-$cycles; $phaseAng:=2.0*Pi*$phaseFrac");
		

		ensureSubpartsExist();

		// This reads a new Vector3f and Color4f object every time, which is expensive, and possibly leaky in
		// some non-obvious way?
		super.updateFromMathSpace(mg);	
	}
	
	@Override public Set<ThingEstimate> getSubEstimates() { 
		ensureSubpartsExist();
		Set<ThingEstimate> subs = new HashSet<ThingEstimate>();
		if (mySelfEstim != null) {
			subs.add(mySelfEstim);
		}
		subs.addAll(myPersonEstims);
		subs.addAll(myStuffEstims);
		return subs;
	}
	public static interface Consumer {

		public void setWorldEstimate(WorldEstimate worldEstim);
	}
}
