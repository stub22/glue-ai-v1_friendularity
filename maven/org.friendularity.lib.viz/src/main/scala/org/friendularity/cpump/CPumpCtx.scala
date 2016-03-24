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

import org.appdapter.core.name.{FreeIdent, Ident}
import org.appdapter.fancy.log.VarargsLogging;

trait CPumpListChanFinder {
	// The PumpCtx is responsible for mapping any input msg to a set of possible listeners.
	// Many/all of the listers may actually choose to ignore the message, and it is acceptable
	// (if not always efficient) for Ctx to return all channels, if it has no heuristic filter.
	def findMsgListenChans[MK <: CPumpMsg](postChan : CPChanPost[MK], postedMsg : MK) : Traversable[CPChanListen[MK]]

}
trait CPumpCtx extends CPumpListChanFinder {
	// PostChans are mainly just for bookeeping and reply/receipt routing, but a pumpCtx can also append 
	// other tracking state as needed.


// 	def postAndForget[MK <: CPumpMsg](postChan : CPChanPost[MK], postedMsg : MK) : Unit = {
		// enq for mapping, or map then enq for each listener
		// forget=> we don't care about receiving or tracking any of the results (immediate or eventual).
	// }

}

trait EZCPumpCtx extends CPumpCtx with VarargsLogging {
	//override def postAndForget[MK <: CPumpMsg](postChan : CPChanPost[MK], postedMsg : MK) : Unit = {
	//}
}

class DullPumpCtx extends EZCPumpCtx {
	private val myChans = new scala.collection.mutable.HashMap[Ident, CPumpChan[DullPumpCtx]] // note outer-variance
	
	protected def allListenChans : Traversable[CPChanListen[_ <: CPumpMsg]] = {
		myChans.values.filter(c =>{ c match {
			case listenChan : CPChanListen [_] => true // [_ <: CPumpMsg]
			case _ => false
		}}).map(_.asInstanceOf[CPChanListen[_ <: CPumpMsg]]).toList
	}
	override def findMsgListenChans[MK <: CPumpMsg](postChan : CPChanPost[MK], postedMsg : MK) : Traversable[CPChanListen[MK]] = {
		val allLCs = allListenChans
		allLCs.filter(_.interestedIn(postChan, postedMsg))
		allLCs.map(_.asInstanceOf[CPChanListen[MK]])
	}

	def makeOnewayListenChan[MK <: CPumpMsg](chanID : Ident, msgClz : Class[MK], 
				adoptrs : Traversable[CPumpAdptr[MK, DullPumpCtx, CPumpMsg]]) : CPChanListen[MK] = { 

		val listenChan = new EZListenChan[MK, DullPumpCtx, CPumpMsg](chanID, this, adoptrs)
		myChans.put(chanID, listenChan)
		// new EZListenChan[MK, _ >: DullPumpCtx, _ <: CPumpMsg](chanID, this, adoptrs)
		// EZListenChan[InMsgKind <: CPumpMsg, CtxType <: CPumpCtx, OutBound <: CPumpMsg](chanID : Ident, ctx : CtxType, 
		// myAdoptrs : Traversable[CPumpAdptr[InMsgKind, _, CtxType]]
		listenChan
	}
	def makeOnewayDispatchPostChan[MK <: CPumpMsg](chanID : Ident, msgClz : Class[MK]) : DispatchPostChan[MK] = {
		val postChan = new EZDispatchPostChan[MK, DullPumpCtx](chanID, this)
		myChans.put(chanID, postChan)
		postChan
	}
	def makeOnewayForwardPostChan[MK <: CPumpMsg](chanID : Ident, msgClz : Class[MK], teller: CPMsgTeller) : ForwardPostChan[MK] = {
		val postChan = new EZForwardPostChan[MK, DullPumpCtx](chanID, this, teller)
		myChans.put(chanID, postChan)
		postChan
	}

	// When a message is addressed by Ident (e.g. from a web request), we can post it directly to the context as follows.
	// True = sent successfully, as far as we know
	// False = not sent, usually because requested post chan does not exist in this ctx.
	def forwardMsgToPostChan[MK <: CPumpMsg](chanID : Ident, msgToPost : MK) : Boolean = {
		val chanOpt : Option[CPumpChan[DullPumpCtx]] = myChans.get(chanID)
		if (chanOpt.isDefined) {
			val chan = chanOpt.get
			if (chan.isInstanceOf[CPChanPost[MK]]) {
				val postChan = chan.asInstanceOf[CPChanPost[MK]]
				postChan.postAndForget(msgToPost)
				debug1("Successfully posted msg to channel at ID={}", chanID)
				true
			} else {
				warn3("On ID={} found chan={}, which does not support required form of post for msg={}", chanID, chan, msgToPost)
				false
			}
		} else {
			warn3("No channel found on ID={} in this ctx={}, so cannot deliver msg={}", chanID, this, msgToPost);
			false
		}
	}
}