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

import org.friendularity.math.api.MathGate;

/**
 *
 * @author Stu B. <www.texpedient.com>
 */
public abstract class Oscillator<OutType> {
	private		String		myMathExpr;
	private		double[]	myBuffer;
	private		OutType		myOutObj;
	
	public Oscillator( String mExpr, int dim, OutType outObj) { 
		myMathExpr = mExpr;
		myBuffer = new double[dim];
		myOutObj = outObj;
	}
	public OutType getOutputObject() { 
		return myOutObj;
	}
	// Override me to use a different computational approach.
	protected void readDoublesIntoBuf(MathGate mathGate, double[] buffer) {
		if (myMathExpr != null) {
			mathGate.parseAndEvalExprToDoubleVec(myMathExpr, buffer);
		}
	}
	// Override me to update an OutType from a buffer
	protected abstract void updateOutObjFromDoublesBuf(OutType outObj, double[] buffer);
		
	// We pass mathGate explicitly to ensure that they can be replaced/interchanged without "breaking" dependent
	// object like Oscillators.
	public void doUpdate(MathGate mathGate) { 
		readDoublesIntoBuf(mathGate, myBuffer);
		updateOutObjFromDoublesBuf(myOutObj, myBuffer);
	}
	public void setMathExpr (String mathExpr) {
		myMathExpr = mathExpr;
	}
}
