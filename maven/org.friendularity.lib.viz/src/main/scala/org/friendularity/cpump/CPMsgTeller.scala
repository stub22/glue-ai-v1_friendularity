package org.friendularity.cpump

import java.io.{Serializable => JSerializable}

import akka.actor.{ActorSelection, ActorRef}

/**
  * Created by Owner on 4/10/2016.
  */
// Tellers may be included in messages.
trait CPMsgTeller extends JSerializable {
	def tellCPMsg(msg: CPumpMsg)
}

// Wrapper for sender who wants an answer
trait CPReceiptTeller extends CPMsgTeller {
	// TODO:  Refine this impl
	def tellCPReceipt(msg: CPReceiptMsg) = tellCPMsg(msg)
}

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
