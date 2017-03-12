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

package org.friendularity.infra.cpump

import org.appdapter.core.name.{FreeIdent, Ident}
import org.appdapter.fancy.log.VarargsLogging
import org.friendularity.infra.cpmsg.CPumpMsg
;

class EZCPumpChan[CtxType <: CPumpCtx](myChanID : Ident, myCtx : CtxType, myBTF_opt : Option[BoundaryTellerFinder])
			extends  CPumpChan[CtxType] with BoundedCPumpChan[CtxType] with VarargsLogging {

	override def getChanIdent : Ident = myChanID
	
	// override protected def getCtx : CtxType = myCtx
	
	protected def getUsualChanMsgType : Class[_ <: CPumpMsg] = classOf[CPumpMsg]
	
	protected def isUsualMsgType(msg : CPumpMsg) : Boolean = {
		val usualMT = getUsualChanMsgType
		usualMT.isInstance(msg)
	}

	override protected def getBoundaryTellerFinder : Option[BoundaryTellerFinder] = myBTF_opt
}

// These three bounds types may be broad or narrow.
// OutBound must be sufficient to contain the aggregated/condensed immediate result of all adoptrs on the chan,
// which is where we embed all receipt+debug info.  It must also itself be a CPumpMsg!
class EZListenChan[InMsgKind <: CPumpMsg, CtxType <: CPumpCtx, OutBound <: CPumpMsg](chanID : Ident, ctx : CtxType, 
			myAdoptrs : Traversable[CPumpAdptr[InMsgKind, CtxType, _]], btf_opt : Option[BoundaryTellerFinder])
		extends  EZCPumpChan[CtxType](chanID, ctx, btf_opt) with BoundedCPChanListen[InMsgKind, CtxType]  {
	
	// Ignores output type
	protected def findAdptrs(mk: InMsgKind) : Traversable[CPumpAdptr[InMsgKind, CtxType, _ ]] = myAdoptrs
	// , outMsgClz : classOf[OutMsgType]
	// This form of processMsg allows for further narrowing of expected output type, by explicit signal class.
 	protected def processRcvdMsg[OMK <: OutBound](inMsg : InMsgKind, outMsgClz:Class[OMK], pumpCtx_opt : Option[CtxType]) : Traversable[OMK] = {
		info2("processRcvdMsg inMsg={} outMsgClz=", inMsg, outMsgClz)
		// val ctx : CtxType = null : CtxType // getCtx
		val adptrs = findAdptrs(inMsg)
		val allOutputs : Traversable[_] = adptrs.flatMap(_.processMsg(inMsg, pumpCtx_opt))
		allOutputs.map(_.asInstanceOf[OMK])
		//  	processMsg(inMsg : InMsgType, pumpCtx : CtxType) : Traversable[OutMsgType]
	}
	private def getDeliveryCtx_opt : Option[CtxType] = None
	override def enqueueAndForget(inMsg : InMsgKind) : Unit = {
		processRcvdMsg(inMsg, null, getDeliveryCtx_opt) // classOf[OutBound])
	}
	override def interestedIn(postChan : CPChanPost[_, CtxType], postedMsg : CPumpMsg) : Boolean = {
		isUsualMsgType(postedMsg)
	}
}

