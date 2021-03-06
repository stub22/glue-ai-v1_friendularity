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

import org.appdapter.core.item.Item
import org.appdapter.core.log.BasicDebugger

import org.appdapter.core.name.Ident
import org.appdapter.core.model.RdfNodeTranslator
import org.cogchar.render.goody.dynamic.{DynaShapeGoody, DynamicGoodySpace}
import org.cogchar.render.sys.registry.RenderRegistryClient

import org.appdapter.fancy.rclient.{RepoClient, RepoClientImpl}
import org.appdapter.fancy.query.{InitialBindingImpl}
import org.appdapter.fancy.repo.{FancyRepo}
import org.appdapter.fancy.model.{ModelClientImpl}
import org.appdapter.xload.rspec.OnlineSheetRepoSpec


abstract class SweetDynaGoody(goodyIdxWithinSpace : Int) extends DynaShapeGoody(goodyIdxWithinSpace : Int) {

	// This method is called by the space to tell us that configuration is available.
	// It is not (necessarily) called on the render thread.
	def reconfigureFromSpecItem(mc : RdfNodeTranslator, specItem : Item) 
	// 
	//	// Textual:   Nickname, Label, Description
		// Label is always displayed (when practical), may be updating frequently.  Description is extra detail,
		// not evaluated unless this object is in focus by user.
		// Nickname is short and usually unchanging.

	override def doFastVWorldUpdate_onRendThrd(rrc : RenderRegistryClient) : Unit = { 
		// getLogger().info("FastUpdate to dynaGoody {} at index {}", Seq(getUniqueName, getIndex) :_*);
	}
}
abstract class SweetDynaSpace(parentDGS : DynamicGoodySpace[_], idxIntoParent : Int, val mySpecGraphID : Ident, val mySpecID : Ident) 
		extends DynamicGoodySpace[SweetDynaGoody](parentDGS, idxIntoParent) {
			
	private var		myPendingSpecItems : Set[Item] = Set()
	private var		mySpecModelClient : RdfNodeTranslator = null
	
	// Occurs on slowUpdate-thread, off the renderThread, usually in actor.receive.
	def    refreshFromModelClient(mc : RdfNodeTranslator) : Unit = {
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
	// Should usually done in an actor.receive, could possibly be done on renderThread when prudent.
	protected def applyPendingSpecItems() : Unit = {
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
	// Done on rendThread, and not in an actor.receive.
	override def doFastVWorldUpdate_onRendThrd(rrc : RenderRegistryClient) {
		super.doFastVWorldUpdate_onRendThrd(rrc)
		// We currently *happen* to be doing this slow update on the rendThrd,
		// but there is no requirement that we do so.
		// TODO:  System throughput would gen be better if this were done in separate actor handler.
		applyPendingSpecItems()
	}
}
