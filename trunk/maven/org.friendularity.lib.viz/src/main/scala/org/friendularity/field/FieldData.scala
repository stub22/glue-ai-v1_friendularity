/*
 *  Copyright 2016 by The Friendularity Project (www.friendularity.org).
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
package org.friendularity.field

import org.appdapter.core.name.{FreeIdent, Ident}
import org.friendularity.vwimpl.IdentHlp

/**
  * Created by Stub22 on 7/11/2016.
  */
// Item and Field are our primary terms to describe app data structures enabled for change-notice events.
// This structure is highly compatible with RDF, but does not strictly require RDF at any boundary.
// Thus implementations are free to use any RDF system, or none.  Generally slower-changing structural
// data benefits from RDF infrastructure, while high-volume regular data (e.g. video frames) benefits much less.

// Item = Resource
// Field = Property, optionally defined via rdfs: or owl:DataProperty, owl:ObjectProperty.
// An owl:AnnotationProperty does not have a field.
// ItemField = Reference to the value of a property at a resource
// The value can be of two main logical forms:
// 1) "Bag" = Set of item-refs = set of Idents = set of URIs
// (implies ObjectProperty value = Unordered  collection of items (but may be tagged with order meaning on a serialization)
// Bag = Collection of resources, formally equiv. to a set.  O

// 2) "Leaf" = Single leaf value, corresponding to an RDF Literal Value at a Functional owl:DataProperty
// We do not support multi-valued data properties directly; instead such a property must be mapped
// to being a set of synthetic items which each have the corresponding field value.

// Reference to a particular item-field, which may be used in several places:
//  A) To register interest in updates to this field of this item
//  B) To mark a piece of change-notice event data as applying to this field of this item
//  C) To mark a request to change the value of the field at this item

// An itemFieldSpec may be seen as an ordered pair of Idents = (absolute) URIs.
trait ItemFieldSpec {
	// We don't allow either of these fields to be optional.  This spec is required to be very specific.
	// Leaving either of these fields as "*", conceptually, is accomplished by other mechanisms.
	def getItemID : Ident
	def getFieldID : Ident
	def makeDirectImpl : ItemFieldSpecDirectImpl = new ItemFieldSpecDirectImpl(getItemID,getFieldID)
}

case class ItemFieldSpecDirectImpl(myItemID : Ident, myFieldID : Ident) extends ItemFieldSpec {
	override def getItemID: Ident = myItemID
	override def getFieldID: Ident = myFieldID
}

// ItemFieldData = A value expression defined on an ItemFieldSpec
trait ItemFieldData {
	def getFieldAddress : ItemFieldSpec
}

// FieldDataLeaf = Single primitive/literal value expression
// Mostly equivalent to an RDF statement triple (s,p,o), with a literal in the o(bject) position.
trait FieldDataLeaf extends ItemFieldData {
	def getData : Any
}
trait TypedFieldDataLeaf[LDT] extends FieldDataLeaf {
	override def getData : Any = getTypedData
	def getTypedData : LDT
}
case class UntypedFieldDataLeafImpl(myFieldAddr : ItemFieldSpec, myDat : Any) extends FieldDataLeaf {
	override def getFieldAddress: ItemFieldSpec = myFieldAddr
	override def getData: Any = myDat
}
case class TypedFieldDataLeafImpl[LDT](myFieldAddr : ItemFieldSpec, myTypedData : LDT) extends TypedFieldDataLeaf[LDT] {
	override def getFieldAddress: ItemFieldSpec = myFieldAddr
	override def getTypedData: LDT = myTypedData
}

// FieldDataItemBag containing values at an ObjectProperty:  at *least* the IDs of the known items,
// with possibly some of their fields embedded recursively (which we call "deep"-ening of the bag).
trait FieldDataBag extends ItemFieldData {
	// Unordered collection of Item-IDs, which may be used as further query keys in get___SubFields
	def getContainedItemIDs : Set[Ident]
	// Input is an itemID prev returned by getContainedItemIDs
	def getSomeSubFields(containedItemID : Ident) : Traversable[ItemFieldData]  // Some Subset

	// Can be an expensive operation if there are a lot of fields on a lot of items.
	def getKnownSubSpecs(recursive : Boolean) : Traversable[ItemFieldSpec] = ???
}
// Contains *only* the IDs.
class ShallowFieldDataBag(myFieldAddr : ItemFieldSpec, myContItemIDs : Set[Ident]) extends FieldDataBag {
	override def getFieldAddress: ItemFieldSpec = myFieldAddr

	override def getContainedItemIDs : Set[Ident] = myContItemIDs
	override def getSomeSubFields(containedItemID : Ident) : Traversable[ItemFieldData]  = Nil
}
class EmptyFieldDataBag(fieldAddr : ItemFieldSpec) extends ShallowFieldDataBag(fieldAddr, Set())
// Contains what shallow has, but allso some of the item fields, possibly recursive.
trait DeeperBagData extends FieldDataBag {
	// Input is an itemID prev returned by getContainedItemIDs
	// def getSomeSubFields(containedItemID : Ident) : Traversable[ItemFieldData]  // Some Subset
}
// Unordered collection of Items, as well as all of their fields, recursively.  Includes all reachable data
trait DeepestBagData extends DeeperBagData {
	// Input is an itemID prev returned by getContainedItemIDs
	def getAllSubFields(containedItemID : Ident) : Traversable[ItemFieldData]  // Exaustive set
}
trait FieldDataFilterFuncs {
	def justFieldDataLeafs(mixedFieldData : Traversable[ItemFieldData]) : Traversable[FieldDataLeaf] =
		mixedFieldData.filter(_.isInstanceOf[FieldDataLeaf]).map(_.asInstanceOf[FieldDataLeaf])

	def justFieldDataBags(mixedFieldData : Traversable[ItemFieldData]) : Traversable[FieldDataBag] =
		mixedFieldData.filter(_.isInstanceOf[FieldDataBag]).map(_.asInstanceOf[FieldDataBag])
}

object VWTestFieldIdents extends IdentHlp {
	// TODO:  These will be generated constants from an ontology.
	// The root items are owl:Individuals, while the properties are owl:ObjectProps and owl:DataProps.

	val ROOT_ITEM_BODIES = new FreeIdent("urn:ROOT_ITEM_BODIES#id")
	val ROOT_ITEM_CAMERAS = new FreeIdent("urn:ROOT_ITEM_CAMERAS#id")
	val ROOT_ITEM_GOODIES = new FreeIdent("urn:ROOT_ITEM_GOODIES#id")

	val PROP_hasBody = new FreeIdent("urn:PROP_hasBody#id")
	val PROP_hasCamera = new FreeIdent("urn:PROP_hasCamera#id")
	val PROP_hasGoody = new FreeIdent("urn:PROP_hasGoody#id")

	val PROP_hasX = new FreeIdent("urn:PROP_hasX#id")
	val PROP_hasY = new FreeIdent("urn:PROP_hasY#id")
	val PROP_hasZ = new FreeIdent("urn:PROP_hasZ#id")
}