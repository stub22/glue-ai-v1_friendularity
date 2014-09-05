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

import org.appdapter.core.log.BasicDebugger
import org.appdapter.core.matdat.OnlineSheetRepoSpec
import org.appdapter.core.store.Repo
import org.appdapter.help.repo.{InitialBindingImpl, RepoClient, RepoClientImpl}
import org.appdapter.impl.store.{FancyRepo, ModelClientImpl}
import org.cogchar.bind.symja.{MathGate, MathSpaceFactory}




object RespirationTest extends VarargsLogging {
	// This is the "Glue Test Data -  Estim Viz Demo" test sheet:
	final val TEST_REPO_SHEET_KEY = "0ArBjkBoH40tndHRFS1JTX200WXNNTjI3MGMxWXBDN1E" 
	final val DFLT_NAMESPACE_SHEET_NUM = 3
	final val DFLT_DIRECTORY_SHEET_NUM = 4
	
	def makeDfltOSRS() : OnlineSheetRepoSpec = { 
		val fileResModelCLs = new java.util.ArrayList[ClassLoader]();
		new OnlineSheetRepoSpec(TEST_REPO_SHEET_KEY, DFLT_NAMESPACE_SHEET_NUM, 
											DFLT_DIRECTORY_SHEET_NUM, fileResModelCLs);
	}
	// import org.cogchar.name.dir.{AssumedQueryDir, AssumedGraphDir};
	def main(args: Array[String]) : Unit = {
		// Backup - if logging is not working, try enabling these two lines.
		// Must enable "compile" or "provided" scope for Log4J dep in order to compile this code.
		// Note that these settings can cause double-logging, if there is a log4j.properties found.
		org.apache.log4j.BasicConfigurator.configure();
		org.apache.log4j.Logger.getRootLogger().setLevel(org.apache.log4j.Level.ALL);
		

		initReposLoadMathEval();  // Chains to call testGoodySpace
		
		// Optional: Wander off into a long looping computation...
		
		testDoubleVecFetch();
	}
	

	def initReposLoadMathEval() : SweetDynaSpace = {
		// Note this calls testGoodySpace at the end
		getLogger().info("Why hello there!  Yes, respiration is the order of the hour...")
		val rspec = makeDfltOSRS();
		val dfltTestRepo = rspec.makeRepo();
		
		//dbRepo.addNamedModel(copyID, lightsModelFromSheet);
		//val copiedModel = dbRepo.getNamedModel(copyID)	
		val dfltTestRC = rspec.makeRepoClient(dfltTestRepo); 
		testMathAndDynaGoodies(dfltTestRepo, dfltTestRC);
	}
	
	def testMathAndDynaGoodies (dfltTestRepo: Repo,  dfltTestRC : RepoClient) : SweetDynaSpace = {

		// This does not diretly create any V-World goodies, just tests the config mechanism
		MathyGoodyTest.testDynaGoodyItemLoad(dfltTestRepo, dfltTestRC)		
	}

	def powerLoad() { 
		// We want to iterate over the subcategories of exprs to load them up.
		// Order within a subcat generally should not matter.  
		// These things all constitute
		//   1) Optional Symbols - Any globals or builtins - not for state
		//   2) Optional Types - including state vector names/patterns.  
		//   These usually come from RDF-based schemas: RDFS, OWL.
		//   3) FuncDef - Transformations und mappings = the essence of our mathematical model.
		//   4) FullExpr (and variants) - A query/calculation result stream node, used for state and as view for display.
		//		Establishing one of these in a context causes actual state to be streamed. 
		//		When we use only some subset of this feature, we can get by with variants that are less than a "full" expr.
		//   5) Application objects used to interface with extern subsystems.  In particular:
		//		a) Joint data stream bindings bound to MechIO robot movement data mapping system.
		//		b) DynamicGoodys bound to HeDSpc V-World (via shared, passive Cogchar v-world API objects).
	}
	
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
