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

import java.io.{Serializable => JSerializable}

import akka.actor.{Cancellable, ActorLogging, Actor, ActorSelection, ActorRef}

/**
  * Created by Owner on 4/10/2016.
  */
// Tellers may be included in messages.
trait CPMsgTeller extends JSerializable {
	def tellCPMsg(msg: CPumpMsg)
}

// Strongly typed, contravariant
trait CPStrongTeller[-MsgType <: CPumpMsg] extends CPMsgTeller {
	// Strongly typed wrapper for the weakly-typed inner tell.
	def tellStrongCPMsg(msg: MsgType) = tellCPMsg(msg)
}

trait CanSchedule {
	protected def getActorRef : ActorRef
/*
	def scheduleRepeatingMsg(msg : CPumpMsg) : Cancellable = {

	}
*/
}
case class ActorRefCPMsgTeller[MsgType <: CPumpMsg](actRef : ActorRef) extends CPStrongTeller[MsgType]  with CanSchedule {
	override def tellCPMsg(cpMsg: CPumpMsg): Unit = {
		actRef ! cpMsg
	}
	override protected def getActorRef : ActorRef = actRef
}

case class ActorSelCPMsgTeller(actSel : ActorSelection) extends CPMsgTeller {
	override def tellCPMsg(cpMsg: CPumpMsg): Unit = {
		actSel ! cpMsg
	}
}
/*
// Nonserializable constructor param for an Actor is passed in thru Props.
class UNUSED_OuterPostActor[MsgKind <: CPumpMsg, CtxType <: CPumpCtx](postChan : CPChanPost[MsgKind,CtxType]) extends Actor with ActorLogging {
	val myPostChan_opt : Option[CPChanPost[MsgKind, CtxType]] = Some(postChan)
	//	var myPostChan_opt : Option[CPChanPost[MsgKind, CtxType]] = None
	//	def setPostChan(pc : CPChanPost[MsgKind, CtxType]) : Unit = {
	//		myPostChan_opt = Option(pc)
	//	}
	protected def deliver(cpmsg : MsgKind): Unit = {
		if (myPostChan_opt.isDefined) {
			myPostChan_opt.get.postAndForget(cpmsg)
		}
	}
	def receive = {
		case cpmsg: MsgKind => deliver(cpmsg)
	}
	def getTeller = new ActorRefCPMsgTeller(self)
}

*/