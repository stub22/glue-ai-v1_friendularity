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

package org.friendularity.gmteach.estimate.impl.visual;

import org.cogchar.bind.symja.MathGate;

import com.jme3.math.Vector3f;
import com.jme3.math.ColorRGBA;
import com.jme3.math.Quaternion;


/**
 * @author Stu B. <www.texpedient.com>
 */

public class VisualItemBase {
	// X X X X X
	private		VisualMathExprLib.Vec3fExprNode			myPosExprNode;
	private		VisualMathExprLib.ColorExprNode			myColorExprNode;
	private		VisualMathExprLib.QuaternionExprNode	myDirectionExprNode;
	
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
	}
	public void useTestPatternExprs() { 
		String selfPosVecExpr = "{5.0^Sin[$phaseAng], 6.0^(2*Cos[$phaseAng]), $phaseFrac}";		
	}
}
