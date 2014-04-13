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

import org.appdapter.core.name.{Ident, FreeIdent}
import org.appdapter.core.item.{Item}
import org.appdapter.core.store.{Repo, InitialBinding, ModelClient }

object Selectors {}
trait Sel[T] {
	// def getOneVal(parentItem : Item) : T
}
trait IndivSel extends Sel[Item] {
	def getAllIndivs(modelCli : ModelClient) : Set[Item]
}
trait PropSel[T] extends Sel[T] {
	def getOneVal(parentItem : Item) : T
}
class TypedIndivSel(val myTypeID: Ident) extends IndivSel {
	override def getAllIndivs(modelCli : ModelClient) : Set[Item] = {
		// Find all resources that are marked with property "rdf:type" equiv to myTypeID
		val typeItem = modelCli.makeItemForIdent(myTypeID)
		val typeProp = modelCli.makeIdentForQName("rdf:type")
		val indivSet = typeItem.getLinkedItemSet(typeProp, Item.LinkDirection.REVERSE);

		import scala.collection.JavaConversions._;
		indivSet.toSet
	}
}
class StringPropSel(val myPropID: Ident) extends PropSel[String] { 
	def getOneVal(parentIndivItem : Item) : String = parentIndivItem.getValString(myPropID, EqnExtractors.NOT_FOUND_EXPR)
}

