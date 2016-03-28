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

package org.friendularity.cpump

import akka.actor.{ActorSelection, ActorRef}
import org.appdapter.core.name.Ident

// Empty (so far) marker trait for all msgs.
trait CPumpMsg extends java.io.Serializable {

}
// Tellers may be included in messages.
trait CPMsgTeller extends java.io.Serializable {
	def tellCPMsg(msg: CPumpMsg)
}

trait CPRepliableMsg extends CPumpMsg {
	def getReplyTeller_opt : Option[CPMsgTeller] = None
}

// Contains regular-shaped buffer streams of data as opaque binary or text
trait CPSignalMsg extends CPumpMsg {

}

// Contains graph metadata, as text or tuples, and possibly system wiring info.
trait CPSymbolMsg extends CPumpMsg {

}
trait CPReceiptMsg extends CPSymbolMsg {
	// def getConfirmedTeller : CPMsgTeller
}
// Wrapper for sender who wants an answer
trait CPReceiptTeller extends CPMsgTeller {
	// TODO:  Refine this impl
	def tellCPReceipt(msg: CPReceiptMsg) = tellCPMsg(msg)
}
// Q:  How well does logging play with serializable?

import org.cogchar.api.thing.ThingActionSpec

trait CPThingActionMsg extends CPSymbolMsg {
	def getThingAction : ThingActionSpec
}

trait  CPReliableThingActionMsg extends CPThingActionMsg with CPRepliableMsg

case class ActorRefCPMsgTeller(actRef : ActorRef) extends CPMsgTeller {
	override def tellCPMsg(cpMsg: CPumpMsg): Unit = {
		actRef ! cpMsg
	}
}
case class ActorSelCPMsgTeller(actSel : ActorSelection) extends CPMsgTeller {
	override def tellCPMsg(cpMsg: CPumpMsg): Unit = {
		actSel ! cpMsg
	}
}

case class TxtSymMsg(mySymTxt : String) extends CPSymbolMsg {

}
case class RepliableTxtSymMsg(mySymTxt : String, myReplyTeller_opt : Option[CPMsgTeller])
			extends CPSymbolMsg with CPRepliableMsg {
	override def getReplyTeller_opt : Option[CPMsgTeller] = myReplyTeller_opt

}
// TODO:  Formally mark this spec as serializable
case class CPTAWrapMsg(mySerialTASpec : ThingActionSpec, myReplyTeller_opt : Option[CPMsgTeller])
			extends CPReliableThingActionMsg {
	override def getThingAction : ThingActionSpec = mySerialTASpec
	override def getReplyTeller_opt : Option[CPMsgTeller] = myReplyTeller_opt
}

case class FoundOuterTellerMsg(chanID : Ident, outerTeller : CPMsgTeller) extends CPReceiptMsg

case class CreatedChanTellerMsg(chanID : Ident, createdTeller : CPMsgTeller) extends CPReceiptMsg {
	def getConfirmedTeller : CPMsgTeller = createdTeller
}
trait CPAdminRequestMsg[CtxBound <: CPumpCtx] extends CPSymbolMsg {
	def processInCtx(ctx : CtxBound)
}
case class CPARM_MakeDullListenChan[LMK <: CPumpMsg](chanID : Ident, listenedMsgClz : Class[LMK],
					  adoptrs : Traversable[CPumpAdptr[LMK, DullPumpCtx, CPumpMsg]])
					extends CPAdminRequestMsg[DullPumpCtx] {
	override def processInCtx(ctx : DullPumpCtx): Unit = {
		val listenChan = ctx.makeOnewayListenChan(chanID, listenedMsgClz, adoptrs)
	}
}


case class CPARM_MakeDullPostDispatchChan[PMK <: CPumpMsg](chanID : Ident, postedMsgClz : Class[PMK],
			receiptTeller: CPReceiptTeller)	extends CPAdminRequestMsg[DullPumpCtx] {
	override def processInCtx(ctx : DullPumpCtx): Unit = {
		val postChan = ctx.makeOnewayDispatchPostChan(chanID, postedMsgClz)
		val receiptMsg = CreatedChanTellerMsg(chanID, postChan.getOuterTeller())
		receiptTeller.tellCPReceipt(receiptMsg)

	}
}

case class CPARM_MakeDullPostForwardChan[PMK <: CPumpMsg](chanID : Ident, postedMsgClz : Class[PMK],
							forwardTeller: CPMsgTeller)
			extends CPAdminRequestMsg[DullPumpCtx] {

	override def processInCtx(ctx : DullPumpCtx): Unit = {
		val postChan = ctx.makeOnewayForwardPostChan(chanID, postedMsgClz, forwardTeller)
	// 	val forwardingTeller : CPMsgTeller = postChan.getForwardingTeller
	//	val receiptMsg = new
	}
}
case class CPARM_LookupChanTeller(chanID : Ident, answerTeller: CPMsgTeller) extends CPAdminRequestMsg[DullPumpCtx] {
	override def processInCtx(ctx : DullPumpCtx): Unit = {
		val chanOpt = ctx.getChan(chanID)
		if (chanOpt.isDefined) {
			val outerTeller = chanOpt.get.getOuterTeller()
			val fotm = new FoundOuterTellerMsg(chanID, outerTeller)
			answerTeller.tellCPMsg(fotm)
		}
	}
}
