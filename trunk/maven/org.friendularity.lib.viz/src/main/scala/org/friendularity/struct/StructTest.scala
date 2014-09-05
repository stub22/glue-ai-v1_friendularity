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

package org.friendularity.struct

import org.appdapter.core.log.BasicDebugger;
import org.appdapter.fancy.log.VarargsLogging

import org.cogchar.bind.symja.{MathGate, MathSpaceFactory}

/**
 * @author Stu B. <www.texpedient.com>
 */

object StructTest  extends VarargsLogging {
	def main(args: Array[String]) : Unit = {
		// Backup - if logging is not working, try enabling these two lines.
		// Must enable "compile" or "provided" scope for Log4J dep in order to compile this code.
		// Note that these settings can cause double-logging, if there is a log4j.properties found.		
		org.apache.log4j.BasicConfigurator.configure();
		org.apache.log4j.Logger.getRootLogger().setLevel(org.apache.log4j.Level.INFO);		
		println("Yep, Scala println works!")
		getLogger().info("Sweet, app logging works!")
		info1("Whooopee, also short-varargs, such as seven={}", new Integer(7))
		testStructs()
		testClumsyMathSource()
		testBetterMathSource()
	}
	def testStructs() { 
		// Make factories for the double-Array sizes we commonly use
		val aodf4 = new	AODFactory(4)
		val aodf3 = new	AODFactory(3)
		val aodf2 = new	AODFactory(2)
		val aodf1 = new	AODFactory(1)
		
		// All fields are of value type: double[1], because that is what the factory knows how to work with.
		val bs1 = new BasicStruct[String, ArrayOfDoubles](aodf1)
		
		// All fields are of value type: double[2]
		val bs2 = new BasicStruct[String, ArrayOfDoubles](aodf2)
		
		val inDat1 = aodf1.make()
		inDat1.myVals(0) = -2.8
		
		info0("one dimensional, initialized : inDat1: " + inDat1)
		
		val inDat2 = aodf2.make()
		inDat2.myVals(1) = 3.7
		info0("inDat2: " + inDat2)
		
		val inDat3 = aodf3.make()
		inDat3.myVals(0) = 9.9
		inDat3.myVals(1) = -8.1
		inDat3.myVals(2) = 7.7
		info0("inDat3: " + inDat3)
		
		info0("bs2 - before write: " + bs2)
		bs2.writeField("pair1", inDat1)
		info0("bs2 - after write-1: " + bs2)
		val inDat22 = aodf2.make()
		inDat22.myVals(1) = -17.5
		bs2.writeField("pair1", inDat22)
		bs2.writeField("pair2", inDat2)
		bs2.writeField("pair3", inDat3)
		info0("bs2 - after write-2: " + bs2)
	}
	def testClumsyMathSource() { 
		val msf = new MathSpaceFactory();
		// val mg : MathGate = msf.makeScriptedMathGate();
		val mg : MathGate = msf.makeUnscriptedMathGate();
		val mgds = new MathGateDoublesSource(mg)
		val testExprDim = 2
		val aodf2 = new	AODFactory(2)
		val bs2 = new BasicStruct[String, ArrayOfDoubles](aodf2)
		
		val smp = new StructMapper[String, ArrayOfDoubles, MathGateExpr[ArrayOfDoubles]]
		val expr11 = new MathGateExpr[ArrayOfDoubles]("{-44.0,33.3}", testExprDim, None, None)
		smp.bindField("v1", expr11, aodf2)
		info0("bs2 - before map-1: " + bs2)
		smp.mapSourceDataToStruct(mgds, bs2)
		info0("bs2 - after map-1: " + bs2)
		val expr12 = new MathGateExpr[ArrayOfDoubles]("7 * {3.5, 2}", testExprDim, None, None)
		val expr22 = new MathGateExpr[ArrayOfDoubles]("{99, -0.05}", testExprDim, None, None)
		smp.bindField("v2", expr22, aodf2)
		smp.bindField("v1", expr12, aodf2)
		info0("bs2 - before map-2: " + bs2)
		smp.mapSourceDataToStruct(mgds, bs2)
		info0("bs2 - after map-2: " + bs2)
		
	}
	def testBetterMathSource() { 

		val msf = new MathSpaceFactory();
		// val mg : MathGate = msf.makeScriptedMathGate();
		val mg : MathGate = msf.makeUnscriptedMathGate();

		val  wildMapper = new MathStructMapper
		
		wildMapper.bindFieldToMathExpr("nutty", 3, "{6, 7.0, -8} - {1,1,1}");
		wildMapper.bindFieldToMathExpr("silly", 2, "0.5 * {3, 9*5}");
		
		val wildStructHandle1 = new MathSourcedStructHandle(mg, wildMapper)
		
		info0("Wild struct handle, before exec: " + wildStructHandle1)
		// No fields exist in the struct right now (even though they exist in the mapper)
		// So, this would throw an exception:
		//info0("result-copy for nutty: " + wildStructHandle1.getResultFieldCopy("nutty"))

		wildStructHandle1.updateSourcedFields
		
		info0("Wild struct handle, after exec: " + wildStructHandle1)
		info0("result-copy for nutty: " + wildStructHandle1.getResultFieldCopy("nutty"))
		info0("result-copy for nutty: " + wildStructHandle1.getResultFieldCopy("silly"))

		val  mapperForTameExprFields = new MathStructMapper
		
		
		//val msm1 = new MathStructMapper()
		//val msw1 = new MathStructWrapper(mg)
	}
}

