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

import org.friendularity.cpmsg.{ActorRefCPMsgTeller, CPMsgTeller, CPumpMsg}

import scala.collection.mutable

// case class SetChanMsg()

// trait AdminMsg

// All pump chans have a context and (one or more) URI=Ident
trait CPumpChan[CtxType <: CPumpCtx] {
	def getChanIdent : Ident

	// Handle anyone can use to async-send messages to me, is not always defined.
	def getOuterTeller_opt() : Option[CPMsgTeller] = None

	// Handle I can use to send messages to my owning context, is not always defined.
	protected def getCtxTeller_opt : Option[CPMsgTeller] = None

	// Delivery for internal
	// protected def deliver(cpmsg : CPumpMsg): Unit = ???

}
trait BoundaryTellerFinder {
	//  CtxTeller = managing teller for the chan, with power to administer it, delete it, etc,
	// can only be seen from inside the ctx.
	def findCtxTellerForChan(chan : CPumpChan[_]) : Option[CPMsgTeller]

	// OuterTeller = used from outside the Ctx to send app messages directly to this chan.
	def findOuterTellerForChan(chan : CPumpChan[_]) : Option[CPMsgTeller]
}

trait BoundedCPumpChan[CtxType <: CPumpCtx] extends CPumpChan[CtxType] {
	protected def getBoundaryTellerFinder : Option[BoundaryTellerFinder] // = None // Override to suppy boundaryFinder

	override protected def getCtxTeller_opt : Option[CPMsgTeller] = {
		getBoundaryTellerFinder.flatMap(_.findCtxTellerForChan(this))
	}

	// TODO:  Cache the teller, also allow it to be found through BoundaryTellerFinder
	override def getOuterTeller_opt() : Option[CPMsgTeller] = {
		myCachedOuterActorRef_opt.map(new ActorRefCPMsgTeller(_))
		// getBoundaryTellerFinder.flatMap(_.findOuterTellerForChan(this))
	}

	var myCachedOuterActorRef_opt : Option[ActorRef] = None

	def notifyOuterActorRef(aref : ActorRef) : Unit = {
		myCachedOuterActorRef_opt = Option(aref)
	}

}

// chanListen instances are supplied from user code, to handle received msgs.
trait CPChanListen[InMsgKind <: CPumpMsg, CtxType <: CPumpCtx] extends CPumpChan[CtxType] {
	// Magic challenge is to find/select/make the correct CPumpAdptr to route each msg.
	// But a simple listen chan could be simply a list of adptrs to try.

	// Pre-taste method
	// Chan must override this to indicate which messages should be enqueued.
	// Framework is responsible for setting
	def interestedIn(postChan : CPChanPost[_, CtxType], postedMsg : CPumpMsg) : Boolean = false

	// Use enqueueAndForget in cases where no (overt) result tracking needed - may shortcut past result-gathering setup.
	def enqueueAndForget(inMsg : InMsgKind) : Unit 	
	
	// TODO:  Add methods/subtraits allowing for explicit results propagation back to message sender, 
	// or to third party downstream.

}

trait BoundedCPChanListen[InMsgKind <: CPumpMsg, CtxType <: CPumpCtx] extends CPChanListen[InMsgKind, CtxType] with BoundedCPumpChan[CtxType]

class ListenChanDirectActor[InMsgKind <: CPumpMsg](chan : BoundedCPChanListen[InMsgKind ,_]) extends Actor with ActorLogging {
	def receive = {
		case cpmsg : InMsgKind => {
			chan.enqueueAndForget(cpmsg)
		}
		case omsg => {
			log.warning("Received non-cpump msg: {}", omsg)
		}
	}
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
trait BoundedCPChanPost[InMsgKind <: CPumpMsg, CtxType <: CPumpCtx] extends CPChanPost[InMsgKind, CtxType] with BoundedCPumpChan[CtxType]

class PostChanDirectActor[InMsgKind <: CPumpMsg](chan : BoundedCPChanPost[InMsgKind,_]) extends Actor with ActorLogging {
	def receive = {
		case cpmsg : InMsgKind => {
			chan.postAndForget(cpmsg)
		}
		case omsg => {
			log.warning("Received non-cpump msg: {}", omsg)
		}
	}
}
