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
import org.cogchar.bind.symja.{MathGate, MathSpaceFactory}

object MathStructMapper {
		// TODO:  Init this array with a loop so we can handle a vector large enough for a character's joint positions, up to say dim=100.
		val theFactories = Array[AODFactory](new AODFactory(1), new AODFactory(2), new AODFactory(3), new AODFactory(4), new AODFactory(5))
		def factoryForDim(baseOneDim : Int) = theFactories(baseOneDim - 1)
		// val aodf2 = factoryForDim(2)
}

class MathStructMapper extends StructMapper [String, ArrayOfDoubles, MathGateExpr[ArrayOfDoubles]] {
	var myVPWrappers = new scala.collection.immutable.HashMap[String,VirtParamExpr[_]]
	
	def bindFieldToMathExpr(fieldName : String, resultDim : Int, mathText : String) {
										 
		val arrayFactoryForField = MathStructMapper.factoryForDim(resultDim)
		//  MathGate implements caching of the actual parsed expr for us, in case of mathText being reused often.
		val optDesc : Option[String] = None
		val underMathExpr = new MathGateExpr[ArrayOfDoubles](mathText, resultDim, optDesc, None);
		// This establishes both the mathExpr binding, and the value-factory binding. 
		bindField(fieldName, underMathExpr, arrayFactoryForField)

		
		// WrappedMGExpr[ArrayOfDoubles,NumType](underMathExpr, optInitRes.getOrElse(null.asInstanceOf[NumType]))
	}
	def makeWrappedExpr[XformedType](fieldName : String, aodcf : Option[AODCompatFactory[XformedType]]) {
		val underMathExpr = getFieldExpr(fieldName) 
		
		//if (aodcf.isDefined) {
		//	val vpWrappedExpr = new VirtParamExpr[XformedType](underMathExpr, aodcf.get)
		//	myVPWrappers -= fieldName 
		//	myVPWrappers += (fieldName -> vpWrappedExpr)
		// }
		
	}
									
	def refreshWrappers() { 
		for ((fk,b) <- myVPWrappers) {
			
		}
	}

}
class MathSourcedStructHandle(val myMathGate : MathGate, structMapper : MathStructMapper) 
		extends MappedStructHandle [String, ArrayOfDoubles, MathGateExpr[ArrayOfDoubles]](structMapper) {
	
	val myCalcDoublesSource = new MathGateDoublesSource(myMathGate)
	override def getDataSource : DataSource[MathGateExpr[ArrayOfDoubles], ArrayOfDoubles] = myCalcDoublesSource

	// TODO:  This is where the wrapper value state needs to be bound and made available to the client
	
	def readResultFieldToAOD(fieldName : String, tgtFV : ArrayOfDoubles) {
		readCachedFieldValue(fieldName, tgtFV)
	}
	def readAndConvertResultField[ResultType](fieldName : String, result : ResultType) {
		// Get the wrapperExpr from the mapper
	}
}
class MathyMappedHandleGroup(val myMathGate : MathGate)  {
	// Some set of handles which are all sourced from the same gate, using the same mapper (and thus the same expressions)
	// Since they may be evaluated/written at different times, the different handles may hold different values.
	// Typically a client or subclass would keep a set of these handles in some collection.
	val		myMapper = new MathStructMapper  
	
	def makeHandle = new MathSourcedStructHandle(myMathGate, myMapper)
}