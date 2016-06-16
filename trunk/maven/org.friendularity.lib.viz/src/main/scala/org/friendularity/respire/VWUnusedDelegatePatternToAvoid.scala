package org.friendularity.respire

import akka.actor.{ActorContext, ActorRef}
import org.appdapter.fancy.log.VarargsLogging
import org.friendularity.mjob.{MsgJobLogic, MsgJobLogicFactory}
import org.friendularity.vwmsg.VWorldRequest

/**
  * Created by Owner on 5/20/2016.
  */

// "Heavy" delegate approach is not recommended, but illustrates an interesting sub-case.
trait VWRequestHeavy extends VWorldRequest with VarargsLogging {
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

abstract class HeavyRequestTwo() extends VWRequestHeavy

case class MakeItDoTwoHeavy() extends HeavyRequestTwo() {
	override def processInsideUnsafe(jobLogic : VWDelegatingJobLogic[_ <: VWRequestHeavy], slf : ActorRef, sndr : ActorRef, actx : ActorContext): Unit = {
		// In principle the jobLogic can contain state that we know how to access.
		// It also allows us to get at the sysMgr.
		val sysMgr = jobLogic.getVWorldSysMgr
		// So, basically anything can be done from inside this method, which is both good and bad.
		// Good:  It gets a new feature implemented in just one class, with no extra factory types.
		// Bad:   It has access to basically any kind of VWorld state, without any enforced narrowing,
		// although we can try to tack some on in the jobLogic and also here, in the message-class hierarchy.
	}

}


// Using the "delegating" approach pushes logic into the received message object itself.
// This approach probably has more drawbacks than advantages.
// However, typing it out as a skeleton, and seeing that compile, helps clarify our type algebra.
trait VWDelegatingJobLogic[-Msg <: VWRequestHeavy] extends VWorldJobLogic[Msg] {
	def getVWorldSysMgr : VWorldSysMgr
}

class VWJobLogicDelegatingImpl[-Msg <: VWRequestHeavy](msgFilterClz : Class[Msg], sysMgr : VWorldSysMgr) extends VWDelegatingJobLogic[Msg] {
	override def processMsgUnsafe(msg : Msg, slf : ActorRef, sndr : ActorRef, actx : ActorContext) : Unit = {
		msg.processInsideUnsafe(this, slf, sndr, actx)
	}
	override def getVWorldSysMgr : VWorldSysMgr = sysMgr
}

// trait VWorldJobLogicFactory extends MsgJobLogicFactory {
// override def makeJobLogic[Msg <: VWorldMsg](msgFilterClz : Class[Msg]) : MsgJobLogic[Msg] = ???
// }
class VWJLDImplFactory(sysMgr : VWorldSysMgr) extends MsgJobLogicFactory[VWRequestHeavy, AnyRef] {
	override def makeJobLogic(jobArg : AnyRef, msgFilterClz : Class[VWRequestHeavy]) : MsgJobLogic[VWRequestHeavy] = {
		new VWJobLogicDelegatingImpl[VWRequestHeavy](msgFilterClz, sysMgr)
	}
}
