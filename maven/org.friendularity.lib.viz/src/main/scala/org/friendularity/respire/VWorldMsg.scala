package org.friendularity.respire

import akka.actor.ActorContext
import org.appdapter.fancy.log.VarargsLogging
import org.friendularity.cpump.{CPMsgTeller, CPumpMsg}

/**
  * Created by Owner on 4/19/2016.
  */
trait VWorldMsg extends CPumpMsg with VarargsLogging
trait VWorldRequest  extends VWorldMsg
trait VWorldNotice extends VWorldMsg

trait VWContentRq extends VWorldRequest {
}
trait RdfMsg {
	def asTurtleString : String

	def asJenaModel(flags_opt: Option[AnyRef]) : AnyRef
	// def asR2goModel : AnyRef
}
trait VWGoodyRqRdf extends VWContentRq  with RdfMsg {
}

trait VWAdminRqMsg extends VWorldRequest {
	def processInSys(sysMgr : VWorldSysMgr, actCtx : ActorContext): Unit
}
case class VWARM_GreetFromPumpAdmin(pumpAdminTeller : CPMsgTeller) extends VWAdminRqMsg {
	override def processInSys(sysMgr : VWorldSysMgr, actCtx : ActorContext) : Unit = {
		info3("Processing {} message with sysMgr={} and actCtx={}", this, sysMgr, actCtx)
	}
}
case class VWARM_FindGoodyTeller(answerTeller: CPMsgTeller) extends VWAdminRqMsg {
	override def processInSys(sysMgr : VWorldSysMgr, actCtx : ActorContext) : Unit = {
		info3("Processing {} message with sysMgr={} and actCtx={}", this, sysMgr, actCtx)
	}
}
case class VWARM_FindPublicTellers(answerTeller: CPMsgTeller) extends VWAdminRqMsg {
	override def processInSys(sysMgr : VWorldSysMgr, actCtx : ActorContext) : Unit = {
		info3("Processing {} message with sysMgr={} and actCtx={}", this, sysMgr, actCtx)
		val pubTellers : VWorldPublicTellers = sysMgr.findPublicTellers
		answerTeller.tellCPMsg(pubTellers)
	}
}