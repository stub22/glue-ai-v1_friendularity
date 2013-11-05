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

import org.cogchar.bind.symja.MathGate;
import org.cogchar.render.goody.dynamic.ShapeAnimator;
import org.cogchar.render.goody.dynamic.VizShape;
import com.jme3.math.Vector3f;
import com.jme3.math.ColorRGBA;
import java.util.HashSet;
import java.util.Set;
import org.appdapter.core.log.BasicDebugger;
import org.appdapter.core.name.Ident;
import org.cogchar.render.sys.registry.RenderRegistryClient;

/**
 * @author Stu B. <www.texpedient.com>
 */
public abstract class ThingEstimate extends BasicDebugger {

	public		Ident							myIdent;
	
	private		OscillatorLib.Vec3fOscillator	myPosOscillator;
	private		OscillatorLib.ColorOscillator	myColorOscillator;
			
	public ThingEstimate(Ident id) {
		myIdent = id;
	}

	public void setPosMathExpr (String mathExpr) {
		if (myPosOscillator == null) {
			myPosOscillator  = new OscillatorLib.Vec3fOscillator(mathExpr);			
		} else {
			myPosOscillator.setMathExpr(mathExpr);
		}
	}
	public void setColorMathExpr(String mathExpr) {
		if (myColorOscillator == null) { 
			myColorOscillator = new OscillatorLib.ColorOscillator(mathExpr);
		} else {
			myColorOscillator.setMathExpr(mathExpr);
		}
	}
	public Ident getIdent()  {
		return myIdent;
	}
	public Set<ThingEstimate> getSubEstimates() { 
		return new HashSet<ThingEstimate>();
	}
	
	public Vector3f getVisualPos() {
		// Dummy impl uses "oscillator"
		if (myPosOscillator != null) {
			Vector3f posVec = myPosOscillator.getVector3f();
			return posVec;
		}
		return null;
	}
	public ColorRGBA getVisualColor() {
		if (myColorOscillator != null) { 
			ColorRGBA color = myColorOscillator.getColor();
			return color;
		}
		return null;
	}

	public void updateFromMathSpace(MathGate mg) {
		if (myPosOscillator != null) {
			myPosOscillator.doUpdate(mg);
		}
		if (myColorOscillator != null) {
			myColorOscillator.doUpdate(mg);
		}

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


	public static abstract class CoordinateFrame {

		public Ident myIdent;
		public CoordinateFrame myParent;
	}
	

}
