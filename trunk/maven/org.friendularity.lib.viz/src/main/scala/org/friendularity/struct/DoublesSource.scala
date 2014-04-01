/*
 *  Copyright 2014 by The Friendularity Project (www.friendularity.org).
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

package org.friendularity.struct
import org.cogchar.bind.symja.MathGate;

// Scala's  Array[Double] compiles to java:  double[]  
class ArrayOfDoubles(val myVals : Array[Double]) extends DataValue  {
	lazy val myStringRep = "AOD[" + myVals.mkString(", ") + "]"
	override def toString(): String = myStringRep
}
class AODFactory(val myArraySize : Int) extends Factory[ArrayOfDoubles] {
	override def makeOne() : ArrayOfDoubles = {
		val a = new Array[Double](myArraySize)
		new ArrayOfDoubles(a)
	}
	override def makeArray(size : Int) : Array[ArrayOfDoubles] = {
		new Array[ArrayOfDoubles](size)
	}
	override def shallowCopyContents(source : ArrayOfDoubles, target: ArrayOfDoubles) {
		source.myVals.copyToArray(target.myVals)
	}
}
class OneDouble(val myVal : Double) extends DataValue {
}
class MathGateDoublesSource(val myMG : MathGate) extends DataSource[MathGateExpr, ArrayOfDoubles] {
	override def read(expr : MathGateExpr, dataValue : ArrayOfDoubles) = {
		myMG.parseAndEvalExprToDoubleVec(expr.myExprString, dataValue.myVals);
	}
}