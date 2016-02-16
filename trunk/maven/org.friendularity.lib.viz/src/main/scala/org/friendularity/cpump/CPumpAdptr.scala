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

trait WritingCtx 

trait WritableRecord

trait WrittenResult


trait CPumpAdptr[InMsgType <: CPumpMsg, OutMsgType <: CPumpMsg, CtxType <: CPumpCtx] {
	// Nonempty result collection => shortcut succeeded
	protected def	attemptShortcut(inMsg : InMsgType, pumpCtx : CtxType) : Traversable[OutMsgType]
	
	protected def mapIn(inMsg : InMsgType, ctx : CtxType) : Traversable[WritableRecord]
	
	protected def write(rec : WritableRecord, wc : WritingCtx) : WrittenResult
	
	protected def mapOut(inMsg : InMsgType, wr : WrittenResult, pumpCtx : CtxType) : Traversable[OutMsgType]
	
	def processMsg(inMsg : InMsgType, pumpCtx : CtxType) : Traversable[OutMsgType] = {
		Nil
	}
}
