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

import org.appdapter.core.name.{Ident}
import org.cogchar.bind.symja.{MathGate}

/*
class MathBlockKind(val myPropID : Ident) {
}
object MathBlockKinds {
}
*/
trait MathBlock { 
	def getDescription : String
	def getMathText : String
	// def getOptResultBuf : Option[] = None
	def evalToDubVec(mg : MathGate, bufOrNull : Array[Double]) : Array[Double] = {
		val mathText = getMathText
		mg.parseAndEvalExprToDoubleVec(mathText, bufOrNull)
	}
}
// Here we pull in a Java incarnation of the feature, not for functionality, but
// rather as a burden to ensure compatibility with alternate implementations branching
// out from the rudimentary Java contract in o.f.api.struct. 
import org.friendularity.api.struct.MathExprNode;

class MathGateExpr[NumType](val myExprString : String, resultDim : Int, val optDesc : Option[String], val optInitRes : Option[NumType])
		// This extension is for type-compatibility (with VisualMathExprLib, in particular)
		// rather than functionality, so far.
		// The null parameter indicates "no initial builtin out-obj
		extends MathExprNode[NumType](myExprString, resultDim, optInitRes.getOrElse(null.asInstanceOf[NumType])) with DataExpr with MathBlock {
			
		
	override def getMathText = myExprString
	override def getDescription = optDesc.getOrElse("NO_DESCRIPTION_AVAIL")

	// Has 2 paths to execution of the expr:  evalToDubVec and   MathGateDoublesSource.
	// Former reads into an optional explicit Array[Double], or allocates a new one to return.
	// Latter knows how to read into (required) existing target dataValue of type: ArrayOfDoubles.
	// read(expr : MathGateExpr, dataValue : ArrayOfDoubles) = {
	//	myMG.parseAndEvalExprToDoubleVec(expr.myExprString, dataValue.myVals);

}
trait Transform[InType, OutType] {
	def updateOutput(input : InType, output : OutType)
}
abstract class WrappedMGExpr[NumType,ResultType](val myMGExpr : MathGateExpr[NumType], val myResultObj : ResultType) 
		extends Transform[NumType,ResultType] {
	
	def evalAndUpdate() { 
		// myMGExpr.
	
		val currNum : NumType = myMGExpr.getOutputObject	
		updateOutput(currNum, myResultObj)
	}
}
class VirtParamExpr[VPType](mgExpr : MathGateExpr[ArrayOfDoubles], aodcf : AODCompatFactory[VPType]) 
		extends WrappedMGExpr[ArrayOfDoubles, VPType](mgExpr, aodcf.make) {
	override def updateOutput(input : ArrayOfDoubles, output : VPType)	{
		aodcf.copyFromAOD(input, output)
	}
}

/*
class BasicMathBlock(myDesc : String, aMathText : String) extends MathGateExpr(aMathText) 
		with MathBlock {
	override def getDescription = myDesc
}
*/