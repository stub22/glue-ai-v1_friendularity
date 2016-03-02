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

// All pump chans have a context and (one or more) URI=Ident
trait CPumpChan[CtxType <: CPumpCtx] {
	def getChanIdent : Ident
	
	protected def getCtx : CtxType
}
// chanListen instances are supplied fr om user code, to handle received msgs.

trait CPChanListen[InMsgKind <: CPumpMsg] {
	// Magic challenge is to find/select/make the correct CPumpAdptr to route each msg.
	// But a simple listen chan could be simply a list of adptrs to try.

	// Use enqueueAndForget in cases where no result tracking needed - may shortcut past result-gathering setup.
	def enqueueAndForget(inMsg : InMsgKind) : Unit 	
	
	// TODO:  Add methods/subtraits allowing for explicit results propagation back to message sender, 
	// or to third party downstream.
	
	// Chan can override this to help routing flow:
	def interestedIn(postChan : CPChanPost[_], postedMsg : CPumpMsg) : Boolean = false
}


/// class DullListenChan
// This is the kind of aggregated result we expect processMsg to yield upon return (i.e. "immediately" in CS parlance).
// That is different from aggregated effect of whatever downstream stuff it triggers.
// Presumably any pointer needed to downstream results is included in the immediate results.
trait ExpectedResult {
	
}
// chanPost instances are supplied by pump code, to allow sending messages, which
// generally translates into storage writes, queued packets, or both.   
// PostChan always has a URI, usually unique locally, but possibly shared globally.
trait CPChanPost[MsgKind <: CPumpMsg] {
	
}
