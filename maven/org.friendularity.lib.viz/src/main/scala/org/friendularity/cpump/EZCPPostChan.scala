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
import org.appdapter.fancy.log.VarargsLogging
import org.friendularity.cpmsg.{CPMsgTeller, CPumpMsg}
;


trait DispatchPostChan[MsgKind <: CPumpMsg, CtxType <: CPumpCtx]  extends CPChanPost[MsgKind, CtxType] {

	def getListChanFinder : CPumpListChanFinder[CtxType]

	override def postAndForget(inMsg : MsgKind) : Unit = {
		val listChanFinder = getListChanFinder
		val listenChans = listChanFinder.findMsgListenChans(this, inMsg)  // These chans have now indicated "interest"
		for (lc <- listenChans) {
			lc.enqueueAndForget(inMsg)
		}
	}

	// When a remote client wants a handle to post to, it can use this (wrapped in ForwardPostChan).
	// lazy val myTeller
}
trait BoundedDispatchPostChan [MsgKind <: CPumpMsg, CtxType <: CPumpCtx]
			extends DispatchPostChan[MsgKind, CtxType] with BoundedCPChanPost[MsgKind, CtxType]


trait ForwardPostChan[MsgKind <: CPumpMsg, CtxType <: CPumpCtx]  extends CPChanPost[MsgKind, CtxType] {
	def getTargetTeller : CPMsgTeller

	override def postAndForget(inMsg : MsgKind) : Unit = {
		val teller = getTargetTeller
		teller.tellCPMsg(inMsg)
	}
}

trait BoundedForwardPostChan[MsgKind <: CPumpMsg, CtxType <: CPumpCtx]
		extends ForwardPostChan[MsgKind, CtxType] with BoundedCPChanPost[MsgKind, CtxType]

case class EZDispatchPostChan[MsgKind <: CPumpMsg, CtxType <: CPumpCtx](chanID : Ident, ctx : CtxType,
					listChanFinder : CPumpListChanFinder[CtxType], btf_opt : Option[BoundaryTellerFinder])
			extends EZCPumpChan[CtxType](chanID, ctx, btf_opt)
			with BoundedDispatchPostChan[MsgKind, CtxType]
{

	override def getListChanFinder : CPumpListChanFinder[CtxType] = listChanFinder

}

case class EZForwardPostChan[MsgKind <: CPumpMsg, CtxType <: CPumpCtx](chanID : Ident, ctx : CtxType,
					   myTgtTeller: CPMsgTeller, btf_opt : Option[BoundaryTellerFinder])
			extends EZCPumpChan[CtxType](chanID, ctx, btf_opt)
						with BoundedForwardPostChan[MsgKind, CtxType] {

	override def getTargetTeller : CPMsgTeller = myTgtTeller

}
