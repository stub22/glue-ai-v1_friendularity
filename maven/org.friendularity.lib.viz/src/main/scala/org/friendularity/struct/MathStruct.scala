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
class MathStructMapper extends StructMapper [String, ArrayOfDoubles, MathGateExpr] {
	def bindFieldToMathExpr(fieldName : String, resultDim : Int, mathText : String) {
		val arrayFactoryForField = MathStructMapper.factoryForDim(resultDim)
		//  MathGate implements caching of the actual parsed expr for us, in case of mathText being reused often.
		val optDesc : Option[String] = None
		val mathExpr = new MathGateExpr(mathText, optDesc)
		// This establishes both the mathExpr binding, and the value-factory binding. 
		bindField(fieldName, mathExpr, arrayFactoryForField)
	}
}
class MathSourcedStructHandle(val myMathGate : MathGate, structMapper : MathStructMapper) 
		extends MappedStructHandle [String, ArrayOfDoubles, MathGateExpr](structMapper) {
	
	val myCalcDoublesSource = new MathGateDoublesSource(myMathGate)
	override def getDataSource : DataSource[MathGateExpr, ArrayOfDoubles] = myCalcDoublesSource

	def readResultField(fieldName : String, tgtFV : ArrayOfDoubles) {
		readCachedFieldValue(fieldName, tgtFV)
	}

	def getResultFieldCopy(fieldName : String) : ArrayOfDoubles = {
		val tgtFV = myMapper.makeFieldVal(fieldName)
		myStruct.readField(fieldName, tgtFV)
		tgtFV
	}
}
class MathyMappedHandleGroup(val myMathGate : MathGate)  {
	// Some set of handles which are all sourced from the same gate, using the same mapper (and thus the same expressions)
	// Since they may be evaluated/written at different times, the different handles may hold different values.
	// Typically a client or subclass would keep a set of these handles in some collection.
	val		myMapper = new MathStructMapper  
	
	def makeHandle = new MathSourcedStructHandle(myMathGate, myMapper)
}