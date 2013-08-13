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
import com.jme3.math.Vector3f;
import com.jme3.math.ColorRGBA;
import org.appdapter.core.log.BasicDebugger;
import org.appdapter.core.name.Ident;
import org.cogchar.render.sys.registry.RenderRegistryClient;

/**
 * @author Stu B. <www.texpedient.com>
 */
public abstract class ThingEstimate extends BasicDebugger {

	public Ident					myIdent;

	public VizShape					myCachedVizObject;
	/*
	private String					myPosVecExpr;	
	private	Vector3f				myPendingPosVec3f;
	
	private  String					myColorVecExpr;
	private	 ColorRGBA				myPendingColor;
	*/

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

	public void updateFromMathSpace(MathGate mg) {
		if (myPosOscillator != null) {
			myPosOscillator.doUpdate(mg);
		}
		if (myColorOscillator != null) {
			myColorOscillator.doUpdate(mg);
		}

		
		// logInfo("Estimate " + myIdent + " of type " + getClass() + " read position: " + myCachedPosVec3f);
		// First goal is to Get X, Y, Z + Rot-X, Rot-Y, Rot-Z
		// Formal but costly approach:  Serialize my previous data into MathSpace, calc update, serialize back out.
		// Alternative is to assume a symbiosis:  MathSpace knows what's up!  In this case, we at least serialize
		// in our own ident, and look up state on that basis.  
			
		// Another way is to define a custom *function* as the data provider. In this way we aren't required
		// to update symbol tables on each pass, only the data that our custom function accesses.
	}

	

	protected void attachSimpleVizObj(Visualizer viz) {
		float initRadius = 5.0f;
		ColorRGBA initColor = ColorRGBA.Red;
		Vector3f basePos = new Vector3f(35.0f, 35.0f, -5.0f);
		myCachedVizObject = new VizShape(myIdent, basePos, initRadius, initColor);
		RenderRegistryClient rrc = viz.getRenderRegistryClient();
		ShapeAnimator sa = viz.getShapeAnimator();
		sa.attachChild_onRendThrd(rrc, myCachedVizObject);
	}

	public void renderAsSillyShape(Visualizer viz, float timePerFrame) {
		if (myCachedVizObject == null) {
			attachSimpleVizObj(viz);
		}
		/*
		double animPhase = (System.currentTimeMillis() % 2000L) / 2000.0 * 2.0 * Math.PI;
		double animFract = Math.sin(animPhase);
		float posOffset = 5.0f * (float) animFract;
		Vector3f offsetVec = new Vector3f(2.0f * posOffset, posOffset, 3.0f * posOffset);
		Vector3f totalVec = basePos.add(offsetVec);
		* 
		*/ 
		if (myPosOscillator != null) {
			Vector3f updatedPosVec = myPosOscillator.getVector3f();
		// 	myPosOscillator.getOutputObject();
			myCachedVizObject.setPosition(updatedPosVec);
		}
	//	if (myPendingColor != null) {
			// myCachedVizObject.setColor(myPendingColor);		
		if (myColorOscillator != null) { 
			ColorRGBA updatedColor = myColorOscillator.getColor();
			myCachedVizObject.setColor(updatedColor);
		}
	}

	public static abstract class CoordinateFrame {

		public Ident myIdent;
		public CoordinateFrame myParent;
	}

	public static class Jme3CoordinateFrame extends CoordinateFrame {
		// Vector loc + Quat rot
	}

	public static abstract class Visualizer {

		private ShapeAnimator myShapeAnimator;

		public ShapeAnimator getShapeAnimator() {
			if (myShapeAnimator == null) {
				RenderRegistryClient rrc = getRenderRegistryClient();
				myShapeAnimator = new ShapeAnimator();
				myShapeAnimator.setupMaterials(rrc);
				myShapeAnimator.enable_onRendThrd(rrc);
			}
			return myShapeAnimator;
		}

		protected abstract RenderRegistryClient getRenderRegistryClient();
	}
}
