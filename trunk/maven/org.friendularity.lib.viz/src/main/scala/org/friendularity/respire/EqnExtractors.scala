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

/** Here we bind to presumed ontology for app objects that contain equations in typical patterns for:
 *		rigid bodies,
 *		cameras (real + virtual),
 *		estimators,
 *		joint curves
 */

object EqnExtractors extends VarargsLogging {
	val NOT_FOUND_EXPR : String = "None"	
	def testMathGraphLoadEval(dfltTestRepo : Repo, dfltTestRC : RepoClient) : Unit = {  
		val spatSheetQN = "ccrti:spatial_sheet_60";
		val mathSheetQN = "ccrti:math_sheet_60";
		val spatGraphID = dfltTestRC.makeIdentForQName(spatSheetQN);   // AssumedGraphDir.estimVizTestCfgGraphQN);
		val mathGraphID = dfltTestRC.makeIdentForQName(mathSheetQN);
		info1("viz spatial graphID = {} ", spatGraphID);
		val spatGraph = dfltTestRepo.getNamedModel(spatGraphID);
		// println("Fetched spat model: " + spatGraph);
		val mathGraph  = dfltTestRepo.getNamedModel(mathGraphID); 
		
		ensurePrefixesAligned(mathGraph)

		val mathMCI : ModelClient = new ModelClientImpl(mathGraph);
		val mathTxtSrc = new MathTextSource(mathMCI)
		
		val msf = new MathSpaceFactory();
		// val mg : MathGate = msf.makeScriptedMathGate();
		val mg : MathGate = msf.makeUnscriptedMathGate();

		// val posExprProp_QN = "hev:expr_pos_vec3f"
		
		// Here we grab two FullExprs and prove we can parse and evaluate math expressions from the input graph.
		val eq1_QN = "hevi:test_01"
		grappleFullExpr(eq1_QN, mg, mathTxtSrc)

		val eq2_QN = "hevi:test_02"
		grappleFullExpr(eq2_QN, mg, mathTxtSrc)
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
	def grappleFullExpr(exprIndivQN: String, mg : MathGate,  mTxtSrc : MathTextSource) = {
		val indivItem = mTxtSrc.findParentItem(exprIndivQN)
		
		debug2("At indiv-QN {} found expr-Item: {}", exprIndivQN, indivItem)		
		evalAndPrint(indivItem, mg, mTxtSrc)
	}
	def evalAndPrint(indivItem: Item, mg : MathGate,  mTxtSrc : MathTextSource) : Array[Double] = { 
		val localDebugName = indivItem.getIdent.getLocalName
		val posExprText =  mTxtSrc.positionExprText(indivItem)
		val posOutDubVec : Array[Double] = mg.parseAndEvalExprToDoubleVec(posExprText, null)	
		
		info3("At local {}, math-expr {} evals to double-vec {}", localDebugName, posExprText, posOutDubVec.deep)	
		
		posOutDubVec
	}
	
	def getAllIndivs(mcSrc : ModelClient) : List[Item] = {
		var incrResult = Nil
		
		incrResult
	}
	
}
abstract class EqnHaver {
}
case class FuncDef(funcDefExpr : String) extends EqnHaver
case class Other extends EqnHaver


trait Sel[T] {
	// def getOneVal(parentItem : Item) : T
}
trait IndivSel extends Sel[Item] {
	def getAllIndivs() : List[Item]
}
trait PropSel[T] extends Sel[T] {
	def getOneVal(parentItem : Item) : T
}
class TypedIndivSel(val myTypeID: Ident) extends IndivSel {
	override def getAllIndivs() : List[Item] = {
		var incrResult = Nil		
		incrResult		
	}
}
class StringPropSel(val myPropID: Ident) extends PropSel[String] { 
	def getOneVal(parentIndivItem : Item) : String = parentIndivItem.getValString(myPropID, EqnExtractors.NOT_FOUND_EXPR)
}
trait Expr { 
}
class MathFunc {	
}
trait EqnProvider {
//	def makeExprs(parentItem : Item, selector : Sel )
}
class MathStringPropSels(val mySymSrcMC : ModelClient) {
	private def makeSel(propQN : String) : StringPropSel = {
		val propID : Ident = mySymSrcMC.makeIdentForQName(propQN)
		new StringPropSel(propID)
	}
	// These String QNames match the equation-text property-names(= sheet column headings) in the source graph.
	private val QN_expr_funcDef				= "hev:expr_funcDef"
	private val QN_expr_pos_vec3f			= "hev:expr_pos_vec3f"
	private val QN_expr_ori_sphtw_vec3f		= "hev:expr_ori_sphtw_vec3f"
	private val QN_expr_color_vec4f			= "hev:expr_color_vec4f"

	// Selectors are made from the propIdents
	val mySel_posVec3f  = makeSel(QN_expr_pos_vec3f)
	val mySel_funcDef  = makeSel(QN_expr_funcDef)
	val mySel_oriSphtwVec3f  = makeSel(QN_expr_ori_sphtw_vec3f)
	val mySel_colorVec4f  = makeSel(QN_expr_color_vec4f)
}

class MathTextSource(val mySymSrcMC : ModelClient) {
	val mySels = new MathStringPropSels(mySymSrcMC)
	// val posExprProp_QN = "hev:expr_pos_vec3f"
	// val posExprProp_ID = mySymSrcMC.makeIdentForQName(posExprProp_QN)
	
	// val posExprPropSel = 
	// new StringPropSel(posExprProp_ID)
	// hev:expr_pos_vec3f	hev:expr_funcDef	hev:expr_ori_sphtw_vec3f	hev:expr_color_vec4f	hev:goodySpace	hev:goodyCount	hev:goodyIndex	hev:spatialForm	
		
	// Here items are thought of as parents for expression properties
	def findParentItem(itemIndivQN : String) : Item = mySymSrcMC.makeItemForQName(itemIndivQN)
	
	def funcDefText(parentIndivItem : Item) : String = mySels.mySel_funcDef.getOneVal(parentIndivItem)
	def positionExprText(parentIndivItem : Item) : String = mySels.mySel_posVec3f.getOneVal(parentIndivItem)
	def orientExprText(parentIndivItem : Item) : String = mySels.mySel_oriSphtwVec3f.getOneVal(parentIndivItem)
	def colorExprText(parentIndivItem : Item) : String = mySels.mySel_colorVec4f.getOneVal(parentIndivItem)
	
	def checkAndPrintExprs(parentIndivItem : Item) {
	
	}
	// parentIndivItem.getValString(posExprProp_ID, EqnExtractors.NOT_FOUND_EXPR)
				
}

