package org.friendularity.respire

import akka.actor.{ActorRef, ActorContext}
import org.appdapter.fancy.log.VarargsLogging
import org.friendularity.cpump.{CPMsgTeller, CPumpMsg}

/**
  * Created by Owner on 4/19/2016.
  */
trait VWorldMsg extends CPumpMsg with VarargsLogging
trait VWorldRequest  extends VWorldMsg {


}
// "Heavy" delegate approach is not recommended, but illustrates an interesting sub-case.
trait VWRequestHeavy extends VWorldRequest {
	// This kind of request includes its own impl method.  This code-shipping is quite flexible and
	// can be efficient in terms of total code-cost for features.   However, it carries several negative
	// design implications.  It is recommended against in akka prog guide.
	//
	// We demonstrate it here as just one possible semi-compliant way to map msg to behavior.
	// It is crucial that this method not attempt to mutate any shared state, or state from client.
	// If we (reasonably) assume that jobLogic is protected by same invoking actor that calls us,
	// then it is safe for this message to cause private state mutation within jobLogic.

	def processInsideUnsafe(jobLogic : VWDelegatingJobLogic[_ <: VWRequestHeavy], slf : ActorRef, sndr : ActorRef, actx : ActorContext): Unit
}
trait VWorldNotice extends VWorldMsg

trait VWContentRq extends VWorldRequest {
}

trait VWCoreRq extends VWorldRequest {
	// Used to talk to internal "VWCore" actor
}
trait VWSceneRq extends VWCoreRq {
	// Describes a change to managed VW scene graph, to be translated (usually by VWCore actor)
	// into calls on JME render thread.
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
// Concept:  Type filtering hooha uses concrete classes.  We expect there will be a case class Msg.
class MakeItDoOne() extends VWorldRequest {
	// No contract methods are required for this relatively *light* request, but it expects server
	// side to have sufficient logic to interpret it.  Generally that server logic will need to know
	// the type name MakeItDoOne, and pass it to some kind of handler.
}
class MakeItDoOneAy() extends MakeItDoOne
class MakeItDoOneBee() extends MakeItDoOne

abstract class HeavyRequestTwo() extends VWRequestHeavy

case class MakeItDoTwoHeavy() extends HeavyRequestTwo() {
	override def processInsideUnsafe(jobLogic : VWDelegatingJobLogic[_ <: VWRequestHeavy], slf : ActorRef, sndr : ActorRef, actx : ActorContext): Unit = {
		// In principle the jobLogic can contain state that we know how to access.
		// It also allows us to get at the sysMgr.
		val sysMgr = jobLogic.getVWorldSysMgr
		// So, basically anything can be done from inside this method, which is both good and bad.
		// Good:  It gets a new feature implemented in just one class, with no extra factory types.
		// Bad:   It has access to basically any kind of VWorld state, without any enforced narrowing,
		// although we can try to tack some on in the jobLogic and also here in the message-class hierarchy.
	}

}


