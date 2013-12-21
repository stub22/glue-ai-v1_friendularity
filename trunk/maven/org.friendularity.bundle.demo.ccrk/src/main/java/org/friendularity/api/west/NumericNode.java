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

/**
 * Wrapper for an arbitrary numeric object.
 * Currently allows the NumberMapper feature to be overridden by subclasses, 
 * but will prefer a delegate mapper if supplied.
 * 
 * @author Stu B. <www.texpedient.com>
 */

public abstract class NumericNode<NumType> implements NumericMapper<NumType> {
	private		NumType		myOutObj;
	
	// The "number" should be equivalent to an array-vector of this many doubles.
	private		double[]	myBuffer;
	
	private		NumericMapper	myOptMapperDelegate;
	
	public NumericNode(int dim, NumType initNumObj) { 
		myBuffer = new double[dim];
		myOutObj = initNumObj;		
	}
	public NumType getOutputObject() { 
		return myOutObj;
	}	

	@Override public  void writeNumericFromDoublesBuf (NumType outObj, double[] buffer) {
		if (myOptMapperDelegate != null) {
			myOptMapperDelegate.writeNumericFromDoublesBuf(outObj, buffer);
		} else {
			writeAnyNumericFromAnyDoublesBuf(outObj, buffer);
		}
	}
	/**
	 * Long-name and protected scope.  This feature is needed only if the NumericNode does not 
	 * have a mapper assigned to it.
	 * @param outObj
	 * @param buffer 
	 */
	protected void writeAnyNumericFromAnyDoublesBuf(NumType outObj, double[] buffer) {
		throw new UnsupportedOperationException("NumericNodes should either set a NumberMapper delegate, or override this method!");
	}
	/**
	 * This is our fundamental binding of a type to the MathGate concept.
	 * Reads our node's doubles from a mathGate into the given buffer.
	 * 
	 * @param mathGate
	 * @param buffer 
	 */
	protected abstract void readDoublesIntoBuf(MathGate mathGate, double[] buffer);
	
	// We pass mathGate explicitly to ensure that they can be replaced/interchanged without "breaking" dependent
	// object like Oscillators.
	public void doUpdate(MathGate mathGate) { 
		readDoublesIntoBuf(mathGate, myBuffer);
		writeAnyNumericFromAnyDoublesBuf(myOutObj, myBuffer);
	}

}
