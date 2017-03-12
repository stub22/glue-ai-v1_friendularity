package org.friendularity.infra.mjob

import akka.actor.{ActorContext, ActorRef}
import org.friendularity.infra.cpmsg.CPumpMsg


/**
  * Created by Owner on 6/16/2016.
  */
object TestMsgJob {

}

class MakeItDoOne() extends CPumpMsg {
	// No contract methods are required for this relatively *light* request, but it expects server
	// side to have sufficient logic to interpret it.  Generally that server logic will need to know
	// the type name MakeItDoOne, and pass it to some kind of handler.
}

class MakeItDoOneAy() extends MakeItDoOne
class MakeItDoOneBee() extends MakeItDoOne


// In general these logic-handler types are superior to delegation to "heavy" code-bearing messages.
// However, in this approach , besides the processMsgUnsafe impl, we must also somewhere define a mapping
// or match-case block saying, in some fashion: "MakeItDoOne   getsProcessedBy  SomethingDoerOne".
// OR, we must assume that client of an actor
// wrapper knows which actor to send to (so then that client knowledge is effectively the "mapping").
class SomethingDoerOne(jobArg : AnyRef) extends MsgJobLogic[MakeItDoOne] {
	override def processMsgUnsafe(msg : MakeItDoOne, slf : ActorRef, sndr : ActorRef, actx : ActorContext) : Unit = ???
}
/*
// Here we show how that even a "heavy" (code-bearing) input can be sent to an explicit handler
class SomethingDoerTwo extends MsgJobLogic[MakeItDoTwoHeavy] {
	override def processMsgUnsafe(msg : MakeItDoTwoHeavy, slf : ActorRef, sndr : ActorRef, actx : ActorContext) : Unit = ???
}
*/
class PhonyMasterFactory extends MasterFactory {
	val factOne = new MsgJobLogicFactory[MakeItDoOne, AnyRef]() {
		override def makeJobLogic(jobArg : AnyRef, msgFilterClz : Class[MakeItDoOne]) : MsgJobLogic[MakeItDoOne] = {
			new SomethingDoerOne(jobArg)
			//msgFilterClz match {
			//	case clzA : classOf[MakeItDoOneAy => {
			//		new VWorldJobLogic[MakeItDoOne]() {
			//
			//		}
			//	}
			// }
		}
	}
	val factPairOne = makeFactoryPair[MakeItDoOne, AnyRef](classOf[MakeItDoOne], factOne)
}