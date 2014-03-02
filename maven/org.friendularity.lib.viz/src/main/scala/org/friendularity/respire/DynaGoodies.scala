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

import org.cogchar.render.goody.dynamic.{DynamicGoody, DynamicGoodySpace}


object DynaGoodies  extends VarargsLogging  {
	def testDynaGoodyItemLoad(repo : Repo, repoClient : RepoClient) : Unit = { 
		val graphQN = "ccrti:math_sheet_60";
		val spaceSpecQN = "hevi:space_01";
		val spaceLink_PropQN = "hev:goodySpace";
		
		val graphID = repoClient.makeIdentForQName(graphQN);
		val mathModel = repo.getNamedModel(graphID)
		val mathModelClient = new ModelClientImpl(mathModel)
		val spaceSpecItem = mathModelClient.makeItemForQName(spaceSpecQN);
		
		val dgs = new DynamicGoodySpace(graphID, spaceSpecItem.getIdent);
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
	}
}
