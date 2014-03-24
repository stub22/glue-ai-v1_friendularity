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

import org.cogchar.render.goody.dynamic.{DynamicGoody, DynamicGoodySpace, DynaShapeGoody}


object DynaGoodies  extends VarargsLogging  {
	def testDynaGoodyItemLoad(repo : Repo, repoClient : RepoClient) : SweetDynaSpace = { 
		val graphQN = "ccrti:math_sheet_60";
		val spaceSpecQN = "hevi:space_01";
		val spaceLink_PropQN = "hev:goodySpace";
		
		val graphID = repoClient.makeIdentForQName(graphQN);
		val mathModel = repo.getNamedModel(graphID)
		val mathModelClient = new ModelClientImpl(mathModel)
		val spaceSpecItem = mathModelClient.makeItemForQName(spaceSpecQN);
		val parentDGS = null;
		val dgs = new SweetDynaSpace(parentDGS, -999, graphID, spaceSpecItem.getIdent);
		info1("Got Goody-Space-Spec Item: {}", spaceSpecItem)

		dgs.refreshModelClient(mathModelClient)
		
		val spaceLink_Prop = mathModelClient.makeIdentForQName(spaceLink_PropQN);
		info1("Space Link Prop: {}", spaceLink_Prop)
		val linkedGSItems = spaceSpecItem.getLinkedItemSet(spaceLink_Prop, Item.LinkDirection.REVERSE);
		info1("linkedGSItems: {}",  linkedGSItems)
		val goodyIndex_PropQN = "hev:goodyIndex";
		val goodyIndex_Prop = mathModelClient.makeIdentForQName(goodyIndex_PropQN);

		import scala.collection.JavaConversions._;	
		for (gsi <- linkedGSItems) {
			info1("Got Goody-Spec Item: {}", gsi)
			val dgIndex_oneBased = gsi.getValInteger(goodyIndex_Prop, -1)
			val dg = dgs.getGoodyAtIndex(dgIndex_oneBased)
			dg.updateFromSpecItem(mathModelClient, gsi);
		}	
		dgs
	}
}
import org.cogchar.bind.symja.MathGate;
class SweetDynaGoody(goodyIdxWithinSpace : Int) extends DynaShapeGoody(goodyIdxWithinSpace : Int) {
	// var			myCachedModelClient : ModelClient
/*
	var			myMathGate : MathGate;
	def refreshMathGate(mg : MathGate) {
		myMathGate = mg;
	}	
*/
	def updateFromSpecItem(mc : ModelClient, specItem : Item) {
		
		// TODO:  These property names need to come from ontology-generated constants.
		val posVecExpr_Prop_QN = "hev:expr_pos_vec3f";
		val colorVecExpr_Prop_QN = "hev:expr_color_vec4f";
		// This resolution (and hence direct dependence on ModelClient mc) could be done "once", on a per-space or global basis.
		val posVecExpr_Prop_ID = mc.makeIdentForQName(posVecExpr_Prop_QN);
		val colorVecExpr_Prop_ID = mc.makeIdentForQName(colorVecExpr_Prop_QN);
		
		// From here on out, we only use Items + Idents (so we're not directly RDF/Jena dependent).
		val posVecExpr = specItem.getValString(posVecExpr_Prop_ID, "{0, 0, 0}");		
		val colorVecExpr = specItem.getValString(colorVecExpr_Prop_ID, "{0, 0, 0, 0}");
		getLogger().info("posVecExpr=" + posVecExpr + ", colorVecExpr=" + colorVecExpr);
	}	
}
class SweetDynaSpace(parentDGS : DynamicGoodySpace[_], idxIntoParent : Int, val mySpecGraphID : Ident, val mySpecID : Ident) 
		extends DynamicGoodySpace[SweetDynaGoody](parentDGS, idxIntoParent) {
			
	override def makeGoody(oneBasedIdx : Integer) : SweetDynaGoody = {
		new SweetDynaGoody(oneBasedIdx)
	}
	def    refreshModelClient(mc : ModelClient) : Unit = {
		// myCachedModelClient = mc;
		val specItem : Item = mc.makeItemForIdent(mySpecID);
		getLogger().info("Got space-specItem: {}", specItem);
		val goodyCount_Prop_QN = "hev:goodyCount";
		val goodyCount_Prop_ID = mc.makeIdentForQName(goodyCount_Prop_QN);
		val goodyCount = specItem.getValInteger(goodyCount_Prop_ID, 0);
		getLogger().info("goodyCount=" + goodyCount);
		resizeSpace(goodyCount);		
		// TODO:  Reload Stuff
	}	
	/*
	protected void reloadConfig(ModelClient mc) { }	
	public void updateStuff(ModelClient mc) { 
		Ident	goodyID = myGoodyID;
	}
	*/

}

	/*	public Ident mapGoodyIndexToID(Integer idx) {		return null;	}
	*/
	// This graph operation belongs in a different layer.
	/*

	public Set<Item> getGoodySpecItems() { 
		Set<Item> specItems = new HashSet<Item>();
		return specItems;
	}
	*/
	// private	ModelClient		myCachedModelClient;
	// private	MathGate		myMathGate;
	// private	Integer			myGoodyCount;
	
	// Currently we need the ModelClient to resolve QNames to idents.
	/*

	*/
	/* public void readSpec
	public Ident getIdent() { 
		if (myGoodyID = null) {
			String qName = "h"
			myGoodyID = new 
		}
	}
		/*
	public static class Spec {
		Ident	myKind;
	}
	*/
	// This goody is knowable to the outside world by its graph-eligible ID:
	// private	Ident					myGoodyID, myTypeID;
	/*
	protected Ident getGoodyID() {
		return myGoodyID;
	} 
	*/
	
 
 
	*/