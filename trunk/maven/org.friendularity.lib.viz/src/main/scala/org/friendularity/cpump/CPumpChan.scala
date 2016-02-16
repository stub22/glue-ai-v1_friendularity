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

trait CPumpChan {
	def getChanIdent : Ident
}
// chanListen instances are supplied from user code, to handle received msgs.
trait CPChanListen[MsgKind <: CPumpMsg] {
	// Magic challenge is to find/select/make the correct CPumpAdptr to route each msg.
	// A simple listen chan could be simply a list of adptrs to try.
}
// chanPost instances are supplied by pump code, to allow sending messages.
// Generally invoking a msg post.
// PostChan always has a URI, usually unique locally, but possibly shared globally.
trait CPChanPost[MsgKind <: CPumpMsg] {
	
}