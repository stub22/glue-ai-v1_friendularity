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

import akka.actor._
import org.appdapter.core.name.{FreeIdent, Ident}
import org.appdapter.fancy.log.VarargsLogging
import java.io.{Serializable => JSerializable}
case class SetChanMsg()

trait AdminMsg

trait Teller[TargetKind, MsgKind <: AnyRef] extends JSerializable {
	protected def getKnownActRef_opt : Option[ActorRef]
	// If invoked in actor context, sender will be implicitly hooked up.
	def tellNowFromImplicitSender(msg: MsgKind) : Boolean = {
		val actRefOpt = getKnownActRef_opt
		if (actRefOpt.isDefined) {
			actRefOpt.get ! msg
			true
		} else false
	}
	def checkAndReport(callback : Function0[Boolean]) = ???
}
case class CachedActorRefTeller[TargetKind, MsgKind <: AnyRef ](myActorRef : ActorRef) extends Teller[TargetKind, MsgKind] {
	val myRefOpt = Option(myActorRef)
	override def getKnownActRef_opt = myRefOpt
}
case class SelectionTeller(myActorSelection : ActorSelection) {

}

trait TellerFactory[TargetKind, MsgKind <: AnyRef] {
	def makeTellerAndActor(actrCtx : ActorContext, props : Props, rcvr : Receiver[MsgKind], reportTo: Teller[_, AdminMsg]): CachedActorRefTeller[TargetKind, MsgKind] = {

		null
	}
}

trait Receiver[MsgKind <: AnyRef] extends JSerializable {
	def doRecieve(msg: MsgKind)
}

class FirstReceiver[MsgKind <: AnyRef] extends Receiver[MsgKind] with VarargsLogging {
	override def doRecieve(msg: MsgKind) {
		info2("Receiver={} got msg={}", this, msg : AnyRef)
	}
}

trait WackyTeller[MsgKind <: AnyRef] extends Teller[Receiver[MsgKind], MsgKind]


class ZoomTeller[MsgKind <: AnyRef] {

}

// All pump chans have a context and (one or more) URI=Ident
trait CPumpChan[CtxType <: CPumpCtx] {
	def getChanIdent : Ident

	// Handle anyone can use to async-send messages to me
	def getOuterTeller() : CPMsgTeller = ???

	// Handle I can use to send messages to context
	protected def getCtxTeller : CtxType = ???

	// Delivery for internal
	protected def deliver(cpmsg : CPumpMsg): Unit = ???

}
object DullCPumpActorFactory extends VarargsLogging {

	val dullAkkaSysName = "dullActorSys01"
	val topDullActorName = "dullCPump01"
	val cpumpEndListenerName = "termDullCPumpSys"

	lazy val dullAkkaSys = ActorSystem(dullAkkaSysName)  // Using case-class cons

	// To review Props for construction of ActorRefs, see pp. 70-71 of the Akka Scala PDF Doc, v2.3.14

	def makeDullTopActor = dullAkkaSys.actorOf(Props[DullPumpTopActor], topDullActorName)

	def makeDullOuterPostActor[MsgKind <: CPumpMsg](parentActCtx: ActorContext, dullCtx : DullPumpCtx,
													postChan : CPChanPost[MsgKind, DullPumpCtx]) = {
		val props = Props(classOf[OuterPostActor[MsgKind, DullPumpCtx]])
		val actRef : ActorRef = parentActCtx.actorOf(props)
	}
}
class OuterPostActor[MsgKind <: CPumpMsg, CtxType <: CPumpCtx](postChan : CPChanPost[MsgKind,CtxType]) extends Actor with ActorLogging {
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
// chanListen instances are supplied fr om user code, to handle received msgs.

trait CPChanListen[InMsgKind <: CPumpMsg, CtxType <: CPumpCtx] extends CPumpChan[CtxType] {
	// Magic challenge is to find/select/make the correct CPumpAdptr to route each msg.
	// But a simple listen chan could be simply a list of adptrs to try.

	// Chan must override this to indicate which messages should be enqueued.
	def interestedIn(postChan : CPChanPost[_, CtxType], postedMsg : CPumpMsg) : Boolean = false

	// Use enqueueAndForget in cases where no result tracking needed - may shortcut past result-gathering setup.
	def enqueueAndForget(inMsg : InMsgKind) : Unit 	
	
	// TODO:  Add methods/subtraits allowing for explicit results propagation back to message sender, 
	// or to third party downstream.

}

// This is the kind of aggregated result we expect processMsg to yield upon return (i.e. "immediately" in CS parlance).
// That is different from aggregated effect of whatever downstream stuff it triggers.
// Commonly instructions for reply are propaated into those downstream messages, but
// in cases where the client may want to synchronously poll, then any pointer needed to
// poll for later downstream results should be included here in the immediate results.
trait ExpectedResult {
	
}
// chanPost instances are supplied by pump code, to allow sending messages, which
// generally translates into storage writes, queued packets, or both.   
// PostChan always has a URI, usually unique locally, but possibly shared globally.

trait CPChanPost[MsgKind <: CPumpMsg, CtxType <: CPumpCtx] extends CPumpChan[CtxType] {
	def postAndForget(postedMsg : MsgKind) : Unit
	// "Post" operations should return quickly, and should not process
	// consequences of the posting during the post call.   Usually this just causes a one-way
	// message to be queued to some local or remote destination, via akka, qpid, or similar means.
	// "Forget" means:  We don't care about directly receiving or tracking any of the results
	// (immediate or eventual).   There may be some kind of implicit reply showing up
	// somewhere, but it is not known to the CPump layer.


}

