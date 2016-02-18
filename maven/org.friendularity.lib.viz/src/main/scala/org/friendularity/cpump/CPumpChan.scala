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

trait CPumpChan[CtxType <: CPumpCtx] {
	def getChanIdent : Ident
	
	protected def getCtx : CtxType
}
class EZCPumpChan[CtxType <: CPumpCtx](myChanID : Ident, myCtx : CtxType) extends CPumpChan[CtxType] {
	override def getChanIdent : Ident = myChanID
	
	override protected def getCtx : CtxType = myCtx
}
// chanListen instances are supplied from user code, to handle received msgs.
trait CPChanListen[InMsgKind <: CPumpMsg] {
	// Magic challenge is to find/select/make the correct CPumpAdptr to route each msg.
	// A simple listen chan could be simply a list of adptrs to try.

	// Use in cases where no result tracking needed - may shortcut past result-gathering setup.
	def enqueueAndForget(inMsg : InMsgKind) : Unit 	
	
}
// These three bounds types may be broad or narrow.
// OutBound must be sufficient to contain the aggregated/condensed immediate result of all adoptrs on the chan,
// which is where we embed all receipt+debug info.  It must also itself be a CPumpMsg!
class EZListenChan[InMsgKind <: CPumpMsg, CtxType <: CPumpCtx, OutBound <: CPumpMsg](chanID : Ident, ctx : CtxType, 
			myAdoptrs : Traversable[CPumpAdptr[InMsgKind, _, CtxType]]) 
		extends  EZCPumpChan[CtxType](chanID, ctx) with CPChanListen[InMsgKind]  {
	
	// Ignores output type
	protected def findAdptrs(mk: InMsgKind) : Traversable[CPumpAdptr[InMsgKind, _, CtxType]] = myAdoptrs
	// , outMsgClz : classOf[OutMsgType]
	// This form of processMsg allows for further narrowing of expected output type, by explicit signal class.
 	def processMsg[OMK <: OutBound](inMsg : InMsgKind, outMsgClz:Class[OMK]) : Traversable[OMK] = {
		val ctx = getCtx
		val adptrs = findAdptrs(inMsg)
		val allOutputs : Traversable[_] = adptrs.flatMap(_.processMsg(inMsg, ctx))
		Nil
		//  	processMsg(inMsg : InMsgType, pumpCtx : CtxType) : Traversable[OutMsgType]
	}
	override def enqueueAndForget(inMsg : InMsgKind) : Unit = {
		processMsg(inMsg, null) // classOf[OutBound])
	}
}
// This is the kind of aggregated result we expect processMsg to yield upon return (i.e. "immediately" in CS parlance).
// That is different from aggregated effect of whatever downstream stuff it triggers.
// Presumably any pointer needed to downstream results is included in the immediate results.
trait ExpectedResult {
	
}
// chanPost instances are supplied by pump code, to allow sending messages.
// Generally invoking a msg post.
// PostChan always has a URI, usually unique locally, but possibly shared globally.
trait CPChanPost[MsgKind <: CPumpMsg] {
	
}

class EZPostChan[MsgKind <: CPumpMsg, CtxType <: CPumpCtx](chanID : Ident, ctx : CtxType) extends EZCPumpChan[CtxType](chanID, ctx) with CPChanPost[MsgKind] 
{
	def postAndForget(inMsg : MsgKind) : Unit = {
		val ctx = getCtx
		ctx.postAndForget(this, inMsg)
	}
}