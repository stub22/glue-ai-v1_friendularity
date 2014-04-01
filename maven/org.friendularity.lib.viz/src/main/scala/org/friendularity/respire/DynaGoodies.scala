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
		val graphID = repoClient.makeIdentForQName(graphQN);
		val mathModel = repo.getNamedModel(graphID)
		val mathModelClient = new ModelClientImpl(mathModel)
		val spaceSpecItem = mathModelClient.makeItemForQName(spaceSpecQN);
		info1("Got Goody-Space-Spec Item: {}", spaceSpecItem)
		val parentDGS = null;
		val dgs = new MathyGoodySpace(parentDGS, -999, graphID, spaceSpecItem.getIdent);
		// This sets the desired size of the space, but does not actually cause the goodies to be created.
		// That happens during update() on the render thread.
		dgs.refreshFromModelClient(mathModelClient)
		dgs
	}
}
import org.cogchar.bind.symja.MathGate;
abstract class SweetDynaGoody(goodyIdxWithinSpace : Int) extends DynaShapeGoody(goodyIdxWithinSpace : Int) {

	// var			myCachedModelClient : ModelClient
	def reconfigureFromSpecItem(mc : ModelClient, specItem : Item) 
	// 
	//	// Textual:   Nickname, Label, Description
		// Label is always displayed (when practical), may be updating frequently.  Description is extra detail,
		// not evaluated unless this object is in focus by user.
		// Nickname is short and usually unchanging.
	//
	
	// This method cannot assume it is executed on the renderThread.
	override def doFastVWorldUpdate_onRendThrd() : Unit = { 
		// getLogger().info("FastUpdate to dynaGoody {} at index {}", Seq(getUniqueName, getIndex) :_*);
	}
	// def initShapeAndAttach
}
abstract class SweetDynaSpace(parentDGS : DynamicGoodySpace[_], idxIntoParent : Int, val mySpecGraphID : Ident, val mySpecID : Ident) 
		extends DynamicGoodySpace[SweetDynaGoody](parentDGS, idxIntoParent) {
			
	var		myPendingSpecItems : Set[Item] = Set()
	var		mySpecModelClient : ModelClient = null
	
/*
 * 
 	override def makeGoody(oneBasedIdx : Integer) : SweetDynaGoody = {
		new SweetDynaGoody(oneBasedIdx)
	}
*/
	// Occurs on slowUpdate-thread, off the renderThread
	def    refreshFromModelClient(mc : ModelClient) : Unit = {
		mySpecModelClient = mc;
		val spaceSpecItem : Item = mc.makeItemForIdent(mySpecID);
		getLogger().info("Got space-specItem: {}", spaceSpecItem);
		val goodyCount_Prop_QN = "hev:goodyCount";
		val goodyCount_Prop_ID = mc.makeIdentForQName(goodyCount_Prop_QN);
		val goodyCount = spaceSpecItem.getValInteger(goodyCount_Prop_ID, 0);
		getLogger().info("About to setDesiredSize of space to {}", goodyCount);
		setDesiredSize(goodyCount);
		val spaceLink_PropQN = "hev:goodySpace";		
		val spaceLink_Prop = mySpecModelClient.makeIdentForQName(spaceLink_PropQN);
		getLogger().info("Space Link Prop: {}", spaceLink_Prop)
		val linkedGSItems = spaceSpecItem.getLinkedItemSet(spaceLink_Prop, Item.LinkDirection.REVERSE);
		getLogger.info("linkedGSItems: {}",  linkedGSItems)
		// import scala.collection.JavaConversions._;	
		import scala.collection.JavaConverters._
		myPendingSpecItems = linkedGSItems.asScala.toSet
	}	
	// Not guaranteed to be on the renderThread
	def applyPendingSpecItems() : Unit = {
		if (mySpecModelClient != null) {
			val goodyIndex_PropQN = "hev:goodyIndex";
			val goodyIndex_Prop = mySpecModelClient.makeIdentForQName(goodyIndex_PropQN);
			for (gsi <- myPendingSpecItems) {
				// Here we use explicit integers for mocking purposes, but normally authors should not need
				// to be aware of the indices of goodies they create.
				val dgIndex_oneBased = gsi.getValInteger(goodyIndex_Prop, -1)
				getLogger().info("Got Goody-Spec Item: {} with index {}", Seq(gsi, dgIndex_oneBased) :_*)
				this.synchronized {
					if (hasGoodyAtIndex(dgIndex_oneBased)) {
						val dg = getGoodyAtIndex(dgIndex_oneBased)
						getLogger().info("Looked up DynaGoody: {}", dg)
						dg.reconfigureFromSpecItem(mySpecModelClient, gsi);
						myPendingSpecItems = myPendingSpecItems - gsi;
					}
				}
			}
		}
	}
	override def doFastVWorldUpdate_onRendThrd() {
		super.doFastVWorldUpdate_onRendThrd()
		applyPendingSpecItems()
	}
}
