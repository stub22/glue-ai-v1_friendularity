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
import org.appdapter.core.name.{Ident, FreeIdent}
import org.appdapter.core.item.{Item}
import org.appdapter.core.store.{Repo, InitialBinding, ModelClient }
import org.appdapter.help.repo.{RepoClient, RepoClientImpl, InitialBindingImpl} 
import org.appdapter.impl.store.{FancyRepo};
import org.appdapter.core.matdat.{SheetRepo,_}
import com.hp.hpl.jena.query.{QuerySolution} // Query, QueryFactory, QueryExecution, QueryExecutionFactory, , QuerySolutionMap, Syntax};
import com.hp.hpl.jena.rdf.model.{Model}
import org.appdapter.core.log.BasicDebugger;

import org.appdapter.impl.store.{ModelClientImpl, ResourceResolver};

import org.friendularity.math.api.{MathGate, MathSpaceFactory}

import org.friendularity.api.goody.{DynamicGoody, DynamicGoodySpace}

object RespirationTest extends BasicDebugger {
	// This is the "Glue_EstimVizDemo" test sheet:
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
		// Must enable "compile" or "provided" scope for Log4J dep in order to compile this code.
		org.apache.log4j.BasicConfigurator.configure();
		org.apache.log4j.Logger.getRootLogger().setLevel(org.apache.log4j.Level.ALL);
		
		testRespiration();
		
		//testWermCalcs();
		
		testDoubleVecFetch();
	}
	def testRespiration() : Unit = {
		getLogger().info("Why hello there!  Yes, respiration is the order of the hour...")
		val rspec = makeDfltOSRS();
		val dfltTestRepo = rspec.makeRepo();
		
		//dbRepo.addNamedModel(copyID, lightsModelFromSheet);
		//val copiedModel = dbRepo.getNamedModel(copyID)	
		val dfltTestRC = rspec.makeRepoClient(dfltTestRepo); 
		val spatSheetQN = "ccrti:spatial_sheet_60";
		val mathSheetQN = "ccrti:math_sheet_60";
		val spatGraphID = dfltTestRC.makeIdentForQName(spatSheetQN);// AssumedGraphDir.estimVizTestCfgGraphQN);
		val mathGraphID = dfltTestRC.makeIdentForQName(mathSheetQN);
		println("viz spatial graphID = " + spatGraphID);
		val spatGraph = dfltTestRepo.getNamedModel(spatGraphID);
		println("Fetched spat model: " + spatGraph);
		val mathGraph  = dfltTestRepo.getNamedModel(mathGraphID); 
		val mathMCI : ModelClient = new ModelClientImpl(mathGraph);
		println("Fetched math model: " + mathGraph);
		val msf = new MathSpaceFactory();
		// val mg : MathGate = msf.makeScriptedMathGate();
		val mg : MathGate = msf.makeUnscriptedMathGate();
		
		val eq1_QN = "hevi:test_01"
		val eq1_Item = mathMCI.makeItemForQName(eq1_QN);
		println("Got eq1_Item : " + eq1_Item)
		
		val exprProp_QN = "hev:expr_pos_vec3f"
		val exprProp_ID = mathMCI.makeIdentForQName(exprProp_QN)
		val eq1_expr = eq1_Item.getValString(exprProp_ID, "NOT_FOUND")
		println("Got eq1_expr : " + eq1_expr)
		
		val outDoubleVec : Array[Double] = mg.parseAndEvalExprToDoubleVec(eq1_expr, null)
		
		println("Math-eval produced array: " + outDoubleVec.deep) 
		
		val eq2_QN = "hevi:test_02"
		val eq2_Item = mathMCI.makeItemForQName(eq2_QN);
		println("Got eq2_Item : " + eq2_Item)
		
		val eq2_expr = eq2_Item.getValString(exprProp_ID, "{-1.0}")
		println("Got eq2_expr : " + eq2_expr)
		
		val outDoubleVec2 : Array[Double] = mg.parseAndEvalExprToDoubleVec(eq2_expr, null)
		
		println("Math-eval produced array: " + outDoubleVec2.deep) 
		// val exprPropQN = "hev:expr"
		// val eq1_expr = 
		testGoodySpace(dfltTestRepo, dfltTestRC)
	}
	
	def testGoodySpace(repo : Repo, repoClient : RepoClient) : Unit = { 
		val graphQN = "ccrti:math_sheet_60";
		val spaceSpecQN = "hevi:space_01";
		val spaceLink_PropQN = "hev:goodySpace";
		
		val graphID = repoClient.makeIdentForQName(graphQN);
		val mathModel = repo.getNamedModel(graphID)
		val mathModelClient = new ModelClientImpl(mathModel)
		val spaceSpecItem = mathModelClient.makeItemForQName(spaceSpecQN);
		
		val dgs = new DynamicGoodySpace(graphID, spaceSpecItem.getIdent);
		println("Got Goody-Space-Spec Item: " + spaceSpecItem)

		dgs.refreshModelClient(mathModelClient)
		
		val spaceLink_Prop = mathModelClient.makeIdentForQName(spaceLink_PropQN);
		println("Space Link Prop" + spaceLink_Prop)
		val linkedGSItems = spaceSpecItem.getLinkedItemSet(spaceLink_Prop, Item.LinkDirection.REVERSE);
		println("linkedGSItems: " + linkedGSItems)
		val goodyIndex_PropQN = "hev:goodyIndex";
		val goodyIndex_Prop = mathModelClient.makeIdentForQName(goodyIndex_PropQN);

		import scala.collection.JavaConversions._;	
		for (gsi <- linkedGSItems) {
			println("Got Goody-Spec Item: " + gsi)
			val dgIndex_oneBased = gsi.getValInteger(goodyIndex_Prop, -1)
			val dg = dgs.getGoodyAtIndex(dgIndex_oneBased)
			dg.updateFromSpecItem(mathModelClient, gsi);
		}
		
	}
import org.friendularity.api.west.{ThingEstimate, WorldEstimate, WorldEstimateRenderModule}	
	def testWermCalcs() : Unit = { 
		val werm : WorldEstimateRenderModule  = new WorldEstimateRenderModule();
		// PumaAppUtils.attachVWorldRenderModule(bundleCtx, werm, null);
		// werm.setupVisualizer(null, null, null);
		// Needs to be done at least once for the selfEstim to exist.
		val msf : MathSpaceFactory = new  MathSpaceFactory();
		val mg : MathGate = msf.makeScriptedMathGate();
		werm.setMathGate(mg);
		val worldEstimID : Ident  = new FreeIdent(WorldEstimate.ESTIM_NS + "world_estim_77");
		val west : WorldEstimate  = new WorldEstimate(worldEstimID);
		
		for (idx <- 0 to 10) {
			println("Loop # " + idx)
		}
	}
	def testDoubleVecFetch() : Unit = {
		val msf : MathSpaceFactory = new  MathSpaceFactory();
		val mg : MathGate = msf.makeUnscriptedMathGate();
		// for difference implied by the "new" in this case, see:
		// http://stackoverflow.com/questions/2700175/scala-array-constructor
		val tgtArray = new Array[Double](4)
		val baseExpr = "{-4.0, 3.0, -2.0, 1.0}";
		val oneHundred = 100
		for (idx <- 1 to oneHundred) {
			var lastDvec : Array[Double] = new Array[Double](0)
			val oneMillion = 3 //  1000000
			val fullExpr = "" + idx + " * " + baseExpr;
			for (jdx <- 0 to oneMillion) {
				val dvec : Array[Double] = mg.parseAndEvalExprToDoubleVec(fullExpr, tgtArray);
				lastDvec = dvec;
			}
			println("Loop # " + idx + " produced " + lastDvec.deep)
		}
	}
}
