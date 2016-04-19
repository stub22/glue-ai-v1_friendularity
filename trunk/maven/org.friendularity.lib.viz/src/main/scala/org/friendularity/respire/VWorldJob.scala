package org.friendularity.respire

import akka.actor.{Props, ActorContext, ActorRef}
import org.appdapter.fancy.log.VarargsLogging
import org.friendularity.cpump.CPumpMsg

/**
  * Created by Owner on 4/19/2016.
  */
// Instance of MsgJobLogic may be stateful or stateless/immutable, as defined by subtypes.
trait MsgJobLogic[Msg <: CPumpMsg] extends VarargsLogging {
	// This method has a lot of authority.
	// May cause messages to other actors, creation of other actors, mutation of internal state.
	// May throw exceptions.
	def processMsgUnsafe(msg : Msg, slf : ActorRef, sndr : ActorRef, actx : ActorContext) : Unit

	// Same as unsafe, except that exceptions are caught, logged and returned.
	def processMsgSafe(msg : Msg, slf : ActorRef, sndr : ActorRef, actx : ActorContext) : Option[Throwable] = {
		var myResultExc : Option[Throwable] = None
		try {
			processMsgUnsafe(msg, slf, sndr, actx)
		} catch {
			case thr : Throwable => {
				myResultExc = Some(thr)
				error3("Exception={} caught in logic={} during process of msg={}", thr, this, msg)
			}
		}
		myResultExc
	}

}

trait VWorldJobLogic[Msg <: VWorldMsg] extends MsgJobLogic[Msg] {
}
trait VWJobLogicMaker {
	protected def makeVWJobActor[Msg <: VWorldMsg](actx : ActorContext, jobActName : String,
												   jobLogic : VWorldJobLogic[Msg]) : ActorRef = {
		val vwjActorProps = Props(classOf[VWorldJobActor[Msg]], jobLogic)
		val vwjActorRef : ActorRef = actx.actorOf(vwjActorProps, jobActName)
		vwjActorRef
	}
}
