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
	package org.friendularity.gmteach.api.west;

import org.friendularity.gmteach.impl.visual.VisualMathExprLib;
import org.cogchar.bind.symja.MathGate;
import org.cogchar.render.goody.dynamic.VizShapeGroup;
import org.cogchar.render.goody.dynamic.VizShape;

import com.jme3.math.Vector3f;
import com.jme3.math.ColorRGBA;
import com.jme3.math.Quaternion;

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
	
	// X X X X X
	private		VisualMathExprLib.Vec3fExprNode			myPosExprNode;
	private		VisualMathExprLib.ColorExprNode			myColorExprNode;
	private		VisualMathExprLib.QuaternionExprNode	myDirectionExprNode;
			
	public ThingEstimate(Ident id) {
		myIdent = id;
	}

	public void setPosMathExpr (String mathExpr) {
		if (myPosExprNode == null) {
			myPosExprNode  = new VisualMathExprLib.Vec3fExprNode(mathExpr);			
		} else {
			myPosExprNode.setMathExpr(mathExpr);
		}
	}
	public void setColorMathExpr(String mathExpr) {
		if (myColorExprNode == null) { 
			myColorExprNode = new VisualMathExprLib.ColorExprNode(mathExpr);
		} else {
			myColorExprNode.setMathExpr(mathExpr);
		}
	}
	public void setDirectionMathExpr(String mathExpr) {
		if (myDirectionExprNode == null) {
			myDirectionExprNode = new VisualMathExprLib.QuaternionExprNode(mathExpr);
		} else {
			myDirectionExprNode.setMathExpr(mathExpr);
		}
	}
	public Ident getIdent()  {
		return myIdent;
	}
	public Set<ThingEstimate> getSubEstimates() { 
		return new HashSet<ThingEstimate>();
	}
	
	public Vector3f getVisualPos() {
		if (myPosExprNode != null) {
			Vector3f posVec = myPosExprNode.getVector3f();
			return posVec;
		}
		return null;
	}
	public ColorRGBA getVisualColor() {
		if (myColorExprNode != null) { 
			ColorRGBA color = myColorExprNode.getColor();
			return color;
		}
		return null;
	}
	public Quaternion getVisualDirection() {
		if (myDirectionExprNode != null) {
			Quaternion dirQuat = myDirectionExprNode.getOutputObject();
			return dirQuat;
		}
		return null;
	}
	public void updateFromMathSpace(MathGate mg) {
		if (myPosExprNode != null) {
			myPosExprNode.doUpdate(mg);
		}
		if (myColorExprNode != null) {
			myColorExprNode.doUpdate(mg);
		}
		if (myDirectionExprNode != null) {
			myDirectionExprNode.doUpdate(mg);
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
