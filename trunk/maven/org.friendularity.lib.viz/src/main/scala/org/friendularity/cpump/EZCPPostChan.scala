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

// class EZCPPostChan extends {}
class EZPostChan[MsgKind <: CPumpMsg, CtxType <: CPumpCtx](chanID : Ident, ctx : CtxType) extends EZCPumpChan[CtxType](chanID, ctx) with CPChanPost[MsgKind] 
{
	def postAndForget(inMsg : MsgKind) : Unit = {
		val ctx = getCtx
		ctx.postAndForget(this, inMsg)
	}
}