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
package org.friendularity.infra.field

import org.appdapter.core.name.Ident
import org.appdapter.fancy.log.VarargsLogging
import org.friendularity.infra.cpmsg.{CPStrongTeller, CPumpMsg}


/**
  * Created by Stub22 on 7/11/2016.
  */
trait ItemDataMsg extends CPumpMsg
trait ItemFieldDataMsg extends ItemDataMsg {
	def getMatchedInterestRegID : Ident
	def getSourceTStampMsec : Long
	def getFieldDataBreadthFirst : List[ItemFieldData] // A subset of the interesting fields, with
	// children following parents.  The highest level bags come first, the lowest leaves come last
	// (or they may be directly enclosed in the bags, when that is easier).
	// This ordering allows receiver to efficiently be sure it has a spot to file the later data.
}
case class MatchedDataMsgImpl(myMatchedInterestRegID : Ident, myTStampMsec : Long,
							  myFieldDataBreadthFirst : List[ItemFieldData]) extends ItemFieldDataMsg {
	override def getMatchedInterestRegID : Ident = myMatchedInterestRegID
	override def getSourceTStampMsec : Long = myTStampMsec
	override def getFieldDataBreadthFirst : List[ItemFieldData] = myFieldDataBreadthFirst
}
trait SourceDataMsg  extends ItemDataMsg {
	def getSourceTStampMsec : Long
	def getFieldDataBreadthFirst : List[ItemFieldData]
}
case class SourceDataMsgImpl(myTStampMsec : Long, myFieldDataBreadthFirst : List[ItemFieldData]) extends SourceDataMsg {
	override def getSourceTStampMsec : Long = myTStampMsec
	override def getFieldDataBreadthFirst : List[ItemFieldData] = myFieldDataBreadthFirst
}
/*
trait ItemFieldDataChgMsg[CType] extends ItemFieldDataMsg {
	def getUpdatedValue : CType
}

case class ItemFieldDataChgMsgImpl[CType](myItemFieldSpec : ItemFieldSpec, upData : CType)
			extends ItemFieldDataChgMsg[CType] {

	override def getUpdatedValue: CType = upData
	override def getFieldSpec: ItemFieldSpec = myItemFieldSpec
}
*/
trait FieldInterestRegMsg extends CPumpMsg {
	def getInterestRegID : Ident // Can be used to cancel this interest, later.
	// Indicates which fields of the item we are explicitly interested in, for keying/matching purposes.
	// isInterestedIn() below may also return true for other fieldSpecs.
	def getInterestingFieldSpecs : Traversable[ItemFieldSpec]
	// Convention:  Empty value => ALL fields.   TODO: Make more formal.
	def getDownstreamTeller: CPStrongTeller[ItemFieldDataMsg] // ItemFieldDataChgMsg[_]]
	def getUpdatePeriodSec : Float

	// Sassy:  Allowing the (stored) interest message to supply a filter predicate
	def isInterestedIn(ifd : ItemFieldData) : Boolean = {
		val itmFldSpec : ItemFieldSpec = ifd.getFieldAddress
		getInterestingFieldSpecs.toIterator.contains(itmFldSpec)
	}
}

case class IntRegMsgImpl(regID : Ident, fieldSpecs : Traversable[ItemFieldSpec],
						 downTeller : CPStrongTeller[ItemFieldDataMsg], upPeriodSec : Float) extends FieldInterestRegMsg {

	override def getInterestRegID: Ident = regID

	// Convention:  Empty value => ALL fields.   TODO: Make more formal.
	override def getInterestingFieldSpecs: Traversable[ItemFieldSpec] = fieldSpecs


	override def getDownstreamTeller: CPStrongTeller[ItemFieldDataMsg] = downTeller


	override def getUpdatePeriodSec: Float = upPeriodSec

	// Can be used to cancel this interest, later.

}

case class DistribFlowControlMsg(active : Boolean) extends CPumpMsg

trait UpstreamTellerAvailabilityNotice extends CPumpMsg {
	def getUpstreamTeller : CPStrongTeller[FieldInterestRegMsg]
	def getKnownItemIDs : Traversable[Ident]
}

case class FieldCallbackRegMsgImpl(cbackRegID : Ident,
		leafRegPairs: Traversable[(ItemFieldSpec, Function1[FieldDataLeaf, Unit])],
		bagRegPairs: Traversable[(ItemFieldSpec, Function1[FieldDataBag, Unit])]) extends CPumpMsg


//class SvcGateImpl (myRRC : RenderRegistryClient, myMatPal : MatPallete)  extends SvcGate {
//	override protected def getRRC: RenderRegistryClient = myRRC
//}

trait MsgToStatusSrc extends CPumpMsg

trait ReportSourceCtrlMsg extends MsgToStatusSrc
case class ReportSrcOpen(chanID : Ident, toWhom : CPStrongTeller[SourceDataMsg], policy : ReportingPolicy)
			extends ReportSourceCtrlMsg

trait StatusTickMsg extends MsgToStatusSrc // Effectively a "notice"
case class ReportingTickChance() extends StatusTickMsg



