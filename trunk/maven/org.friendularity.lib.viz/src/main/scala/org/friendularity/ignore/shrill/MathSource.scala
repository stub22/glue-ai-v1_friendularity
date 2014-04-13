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

package org.friendularity.shrill
import org.appdapter.core.store.{Repo, InitialBinding, ModelClient }
import org.appdapter.help.repo.{RepoClient, RepoClientImpl, InitialBindingImpl} 
import org.cogchar.bind.symja.{MathGate, MathSpaceFactory}
import org.cogchar.api.space.{TextVal}

import org.appdapter.core.item.{Item}
import org.appdapter.core.name.{Ident, FreeIdent}

/*
object MathSource {
}
*/
class MathGraphBinding(mathMCI : ModelClient) {
//	 This class is the beginning of a low-level abstraction of what EqnExtractors test code was doing previously,
//		but is too general/low-level, will probably be axed soon.
		val myMathTxtSrc = new MathTextSource(mathMCI)
		
		private val msf = new MathSpaceFactory();
		val myGate : MathGate = msf.makeUnscriptedMathGate();
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

// MathTextSource is a rather shrill extractor for a certain expected categories of math expressions found in 
// a source graph.   It mainly knows ho to get particular source properties from a given item; so essentially
// it just encapsulates the names of these properties and the ability to traverse them from a given "parent item".
// Because it supplies a fixed set of these encapsulating methods, it has a narrow view of what properties are
// available and interesting.  This view is the same as that encoded in MathStringPopSels above.
// 
// The MathTextSource also has the ability to find all the parent items of a particular type, or to find
// a particular item based on its QName.

class MathTextSource(val mySymSrcMC : ModelClient) {
	val myPropSels = new MathStringPropSels(mySymSrcMC)
	
	val myFuncDefIndivSel = new TypedIndivSel(getTypeID("hev:FuncDef"))	
	val myFullExprIndivSel = new TypedIndivSel(getTypeID("hev:FullExpr"))
	
	def getTypeID(typeQN : String) : Ident = {
		mySymSrcMC.makeIdentForQName(typeQN)
	} 
		
	// Here items are thought of as parents for expression-block properties
	def findParentItem(itemIndivQN : String) : Item = mySymSrcMC.makeItemForQName(itemIndivQN)
	
	// Some useful set of functions to load, not itself a result-producing node
	def funcDefText(parentIndivItem : Item) : String = myPropSels.mySel_funcDef.getOneVal(parentIndivItem)
	
	// Most common result production nodes for goody and camera-like things:  Position, Orientation, and Color
	def positionExprText(parentIndivItem : Item) : String = myPropSels.mySel_posVec3f.getOneVal(parentIndivItem)
	def orientExprText(parentIndivItem : Item) : String = myPropSels.mySel_oriSphtwVec3f.getOneVal(parentIndivItem)
	def colorExprText(parentIndivItem : Item) : String = myPropSels.mySel_colorVec4f.getOneVal(parentIndivItem)
	
	def checkAndPrintCommonExprs(parentIndivItem : Item) {	}
	
	def getIndivsMatchingTypeSel(itsel : TypedIndivSel) : Set[Item] = {
		itsel.getAllIndivs(mySymSrcMC)
	}
}