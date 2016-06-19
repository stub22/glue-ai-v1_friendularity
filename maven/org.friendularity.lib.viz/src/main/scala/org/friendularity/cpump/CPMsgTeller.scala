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