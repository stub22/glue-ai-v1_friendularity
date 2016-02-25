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

trait CPumpCtx {
	// PostChans are mainly just for bookeeping and reply/receipt routing, but a pumpCtx can also append 
	// other tracking state as needed.
	// The PumpCtx is responsible for mapping any input msg to a set of possible listeners.
	// Many/all of the listers may actually choose to ignore the message, and it is acceptable
	// (if not always efficient) for Ctx to return all channels, if it has no heuristic filter.
	protected def findMsgListenChans[MK <: CPumpMsg](postChan : CPChanPost[MK], postedMsg : MK) : Traversable[CPChanListen[MK]]
	

	def postAndForget[MK <: CPumpMsg](postChan : CPChanPost[MK], postedMsg : MK) : Unit = {
		// enq for mapping, or map then enq for each listener
		// forget=> we don't care about receiving or tracking any of the results (immediate or eventual).
	}

}

trait EZCPumpCtx extends CPumpCtx{
	override def postAndForget[MK <: CPumpMsg](postChan : CPChanPost[MK], postedMsg : MK) : Unit = {
		val listenChans = findMsgListenChans(postChan, postedMsg)
		for (lc <- listenChans) {
			
		}
	}
}

class DullPumpCtx extends EZCPumpCtx {
	val myChans = new scala.collection.mutable.HashMap[Ident, CPumpChan[DullPumpCtx]] // note outer-variance
	
	protected def allListenChans : Traversable[CPChanListen[_ <: CPumpMsg]] = {
		myChans.values.filter(c =>{ c match {
			case listenChan : CPChanListen [_] => true // [_ <: CPumpMsg]
			case _ => false
		}}).map(_.asInstanceOf[CPChanListen[_ <: CPumpMsg]]).toList
	}
	override protected def findMsgListenChans[MK <: CPumpMsg](postChan : CPChanPost[MK], postedMsg : MK) : Traversable[CPChanListen[MK]] = {
		val allLCs = allListenChans
		allLCs.filter(_.interestedIn(postChan, postedMsg))
		Nil
	}
	// 
	def makeOnewayListenChan[MK <: CPumpMsg](chanID : Ident, msgClz : Class[MK], 
				adoptrs : Traversable[CPumpAdptr[MK, DullPumpCtx, CPumpMsg]]) : CPChanListen[MK] = { 
// 				adoptrs : Traversable[CPumpAdptr[MK, _ >: DullPumpCtx,  _]]) : CPChanListen[MK] = { 

		val listenChan = new EZListenChan[MK, DullPumpCtx, CPumpMsg](chanID, this, adoptrs)
		myChans.put(chanID, listenChan)
		// new EZListenChan[MK, _ >: DullPumpCtx, _ <: CPumpMsg](chanID, this, adoptrs)
		// EZListenChan[InMsgKind <: CPumpMsg, CtxType <: CPumpCtx, OutBound <: CPumpMsg](chanID : Ident, ctx : CtxType, 
			// myAdoptrs : Traversable[CPumpAdptr[InMsgKind, _, CtxType]]
		listenChan
	}
	def makeOnewayPostChan[MK <: CPumpMsg](chanID : Ident, msgClz : Class[MK]) : CPChanPost[MK] = {
		val postChan = new EZPostChan[MK, DullPumpCtx](chanID, this)
		myChans.put(chanID, postChan)
		postChan
	}
}