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

import org.cogchar.bind.symja.{MathGate, MathSpaceFactory}


/**
 * @author Stu B. <www.texpedient.com>
 */

object StructTest  extends org.friendularity.respire.VarargsLogging {
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
		testMathSource()
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
		
		info0("inDat1: " + inDat1)
		
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
	def testMathSource() { 
		val msf = new MathSpaceFactory();
		// val mg : MathGate = msf.makeScriptedMathGate();
		val mg : MathGate = msf.makeUnscriptedMathGate();
		val mgds = new MathGateDoublesSource(mg)
		val aodf2 = new	AODFactory(2)
		val bs2 = new BasicStruct[String, ArrayOfDoubles](aodf2)
		
		val smp = new StructMapper[String, ArrayOfDoubles, MathGateExpr]
		val expr11 = new MathGateExpr("{-44.0,33.3}")
		smp.bindField("v1", expr11, aodf2.make())
		info0("bs2 - before map-1: " + bs2)
		smp.mapSourceDataToStruct(mgds, bs2)
		info0("bs2 - after map-1: " + bs2)
		val expr12 = new MathGateExpr("7 * {3.5, 2}")
		val expr22 = new MathGateExpr("{99, -0.05}")
		smp.bindField("v2", expr22, aodf2.make())
		smp.bindField("v1", expr12, aodf2.make())
		info0("bs2 - before map-2: " + bs2)
		smp.mapSourceDataToStruct(mgds, bs2)
		info0("bs2 - after map-2: " + bs2)
		
	}
}

