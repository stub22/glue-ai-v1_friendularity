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

package org.friendularity.respire
import org.appdapter.core.name.{Ident, FreeIdent}
import org.appdapter.core.item.{Item}
import org.appdapter.core.store.{Repo, InitialBinding, ModelClient }
import org.appdapter.help.repo.{RepoClient, RepoClientImpl, InitialBindingImpl} 
import org.appdapter.impl.store.{FancyRepo};
import org.appdapter.core.matdat.{SheetRepo, OnlineSheetRepoSpec}
import com.hp.hpl.jena.query.{QuerySolution} // Query, QueryFactory, QueryExecution, QueryExecutionFactory, , QuerySolutionMap, Syntax};
import com.hp.hpl.jena.rdf.model.{Model}
import org.appdapter.core.log.BasicDebugger;

import org.appdapter.impl.store.{ModelClientImpl, ResourceResolver};

import org.cogchar.bind.symja.{MathGate, MathSpaceFactory}
import org.cogchar.api.space.{TextVal}

import org.friendularity.struct.{MathBlock}

/** Here we bind to presumed ontology for app objects that contain equations in typical patterns for:
 *		rigid bodies,
 *		cameras (real + virtual),
 *		estimators,
 *		joint curves
 */


object EqnExtractors extends VarargsLogging {
	val NOT_FOUND_EXPR : String = "None"	
	def testMathGraphLoadEval(dfltTestRepo : Repo, dfltTestRC : RepoClient, mathSrcGraphQN : String) : Unit = {  
		// val spatSheetQN = "ccrti:spatial_sheet_60";
		val mathSheetQN = mathSrcGraphQN; // "ccrti:math_sheet_60";
		// val spatGraphID = dfltTestRC.makeIdentForQName(spatSheetQN);   // AssumedGraphDir.estimVizTestCfgGraphQN);
		val mathGraphID = dfltTestRC.makeIdentForQName(mathSheetQN);
		// info1("viz spatial graphID = {} ", spatGraphID);
		// val spatGraph = dfltTestRepo.getNamedModel(spatGraphID);
		// println("Fetched spat model: " + spatGraph);
		val mathGraph  = dfltTestRepo.getNamedModel(mathGraphID); 
		
		ensurePrefixesAligned(mathGraph)

		
		val mathMCI : ModelClient = new ModelClientImpl(mathGraph);
		val mgb = new MathGraphBinding(mathMCI)
	//	val mathTxtSrc = new MathTextSource(mathMCI)
		
	//	val msf = new MathSpaceFactory();
	//	val mg : MathGate = msf.makeUnscriptedMathGate();

		testMathWithKnownItems(mgb.myGate, mgb.myMathTxtSrc)
		
		findMathItemsByType(mgb.myMathTxtSrc)
	}
	def findMathItemsByType(mathTxtSrc : MathTextSource) { 
		
		val allFuncDefs = mathTxtSrc.getIndivsMatchingTypeSel(mathTxtSrc.myFuncDefIndivSel)
		val allFullExprs = mathTxtSrc.getIndivsMatchingTypeSel(mathTxtSrc.myFullExprIndivSel)
		
		info1("All funcDefs size: {}", allFuncDefs.size : java.lang.Integer)
		for (fdi : Item <- allFuncDefs) {
			val funcDefText : String =  mathTxtSrc.funcDefText(fdi)
			info2("Found funcDef at {} with Text: {}", fdi, funcDefText)
		}
		
		info1("All fullExprs size: {}", allFullExprs.size : java.lang.Integer)
		for (xi : Item <- allFullExprs) {
			val posExprText : String =  mathTxtSrc.positionExprText(xi)
			info2("At {}, found posExpr-text: {}", xi, posExprText)
			val orientExprText : String =  mathTxtSrc.orientExprText(xi)
			info2("At {}, found orientExpr-text: {}", xi, orientExprText)
			val colorExprText : String =  mathTxtSrc.colorExprText(xi)
			info2("At {}, found colorExpr-text: {}", xi, colorExprText)
		}		
	}
	def ensurePrefixesAligned(mathGraph : Model) : Unit = {
		val mathModelPrefixMap = mathGraph.getNsPrefixMap()
		// println("Fetched math model: " + mathGraph);
		debug0("\n\n*************************************************************");
		debug1("Fetched math prefix-map - if this is empty, then all the QName-resolves below will fail: {} ", mathModelPrefixMap)
		debug0("*************************************************************\n\n");
		
		if (mathModelPrefixMap.size() == 0) {
			//Set prefixes manually 
			mathGraph.setNsPrefix("hev", "urn:ftd:headyspace.org:2013:estimviz_type#");
			mathGraph.setNsPrefix("hevi", "urn:ftd:headyspace.org:2013:estimviz_inst#");
		}		
	}
	def testMathWithKnownItems(mg : MathGate,  mathTxtSrc : MathTextSource) { 
		// Here we grab two FullExprs with known QNames and prove we can parse and evaluate math expressions from the input graph.
		val (eq1_QN, eq2_QN) = ("hevi:test_01", "hevi:test_02")
		findEvalAndPrintPosExpr(eq1_QN, mg, mathTxtSrc)
		findEvalAndPrintPosExpr(eq2_QN, mg, mathTxtSrc)		
	}
	def findEvalAndPrintPosExpr(exprIndivQN: String, mg : MathGate,  mTxtSrc : MathTextSource) = {
		val indivItem = mTxtSrc.findParentItem(exprIndivQN)
		
		debug2("At indiv-QN {} found expr-Item: {}", exprIndivQN, indivItem)		
		evalAndPrintPos(indivItem, mg, mTxtSrc)
	}
	def evalAndPrintPos(indivItem: Item, mg : MathGate,  mTxtSrc : MathTextSource) : Array[Double] = { 
		val localDebugName = indivItem.getIdent.getLocalName
		val posExprText =  mTxtSrc.positionExprText(indivItem)
		val posOutDubVec : Array[Double] = mg.parseAndEvalExprToDoubleVec(posExprText, null)	
		
		info3("At local {}, .pos-expr {} evals to double-vec {}", localDebugName, posExprText, posOutDubVec.deep)	
		posOutDubVec
	}
	def makeEqnTextVal() : TextVal = {
		val tvCand = new TextVal("some eqn text");
		tvCand
	}
	def updateResultTextVal(toUpdate : DubArrTextVal, resDubArr : Array[Double]) : Unit = {
		
	}
	
}

class DubArrTextVal extends TextVal("{3.14, -99, 22}") {
	def txtVal() = "wow"
}

// trait Expr { 
// }

class LoadedTextBlock(val myGraphID : Ident, val myItemID : Ident, val myTextPropID : Ident, val myLoadTStamp : Long, val myText : String) {	
	def getDescription : String = { "LTB[gid=" + myGraphID + ", ...]" }
}

class LoadedMathBlock(graphID : Ident, itemID : Ident, textPropID : Ident, loadTStamp : Long, text : String) 
		extends LoadedTextBlock(graphID, itemID, textPropID, loadTStamp, text) with MathBlock {
	
	override def getMathText = myText	
}
class MathBlockPropLoader(myPropSel : StringPropSel) { 
}
class FuncDefBlock(myItemID : Ident, val myText : String) {
}
class MathFunc {	
}
trait EqnProvider {
//	def makeExprs(parentItem : Item, selector : Sel )
}


