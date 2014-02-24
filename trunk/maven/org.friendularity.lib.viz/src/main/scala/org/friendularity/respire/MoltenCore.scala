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

package org.friendularity.respire

/**
 * A molten core is hot in the sense of being chaotic, interconnected, active computing.
 * Usually, such computing uses significant resources, consuming juice and generating heat.
 * Our goal is to maximize the ratio of output utility ("light") to this heat cost.
 * @author Stu B. <www.texpedient.com>
 */

import org.cogchar.bind.symja.{MathGate, MathSpaceFactory}
import org.appdapter.core.store.{Repo, InitialBinding, ModelClient }

class MathCore(val myMathGate : MathGate, val myMathGraphMC : ModelClient ) extends VarargsLogging {
	
}
class MiltonBore  extends VarargsLogging {
}
class MoltenCore  extends VarargsLogging {
	def testDoubleVecFetch() : Unit = {
		val msf : MathSpaceFactory = new  MathSpaceFactory();
		val mg : MathGate = msf.makeUnscriptedMathGate();
		// for difference implied by the "new" in this case, see:
		// http://stackoverflow.com/questions/2700175/scala-array-constructor
		val tgtArray = new Array[Double](4)
		val baseExpr = "{-4.0, 5/2, 14 /-7, Sqrt[1.001]}";
		val oneHundred = 100
		for (idx <- 1 to oneHundred) {
			var lastDvec : Array[Double] = new Array[Double](0)
			val oneMillion = 1000000
			// Create a string expr multiplying index (scalar, integer) and baseExpr (vector of floats + ints) 
			val fullExpr = "" + idx + " * " + baseExpr;
			for (jdx <- 0 to oneMillion) {
				val dvec : Array[Double] = mg.parseAndEvalExprToDoubleVec(fullExpr, tgtArray);
				lastDvec = dvec;
			}
			val idxObject : java.lang.Integer = idx
			info2("Loop # {} produced {}", idxObject, lastDvec.deep)
		}
	}
}
