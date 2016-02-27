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

trait WrittenResult {
	
}
// Note that an Adptr knows how to use a supplied ctx method arg, but it may not keep its own ctx ref.
// Adptrs should be stateless and threadsafe, except for the effects of the write method, but even those
// effects should all be routed through the wc WritingCtx.
// 
// Adptr design is somewhat monadic:  See the flow in processMsg.
trait CPumpAdptr[-InMsgType <: CPumpMsg, -CtxType <: CPumpCtx, +OutMsgType <: CPumpMsg] {
	// Nonempty result collection => shortcut succeeded
	protected def	attemptShortcut(inMsg : InMsgType, pumpCtx : CtxType) : Traversable[OutMsgType]
	
	
	protected def mapIn(inMsg : InMsgType, ctx : CtxType) : Traversable[WritableRecord]
	
	// There could be a delay between call to mapIn and call to write, and that could cause problems.
	// TODO:  Define further restriction to make that possibility go away.
	
	// WrittenResult supplies everything the downstream explicitly knows about the operation.
	protected def write(rec : WritableRecord, wc : WritingCtx) : WrittenResult // Stateful activity limited to here
	
	// There may be a delay between the call to write and the call to mapOut.
	// 
	// mapOut does not get to see the WritableRecord; to find state it must sepatately use 
	protected def mapOut(inMsg : InMsgType, wresults : Traversable[WrittenResult], pumpCtx : CtxType) : Traversable[OutMsgType]
	
	def processMsg(inMsg : InMsgType, pumpCtx : CtxType) : Traversable[OutMsgType] = {
		
		val shortcutResults = attemptShortcut(inMsg, pumpCtx)
		if (shortcutResults.nonEmpty) {
			shortcutResults
		} else {
			val writableRecs = mapIn(inMsg, pumpCtx)
			val writtenResults : Traversable[WrittenResult] = if (writableRecs.nonEmpty) {
				val wc : WritingCtx = null
				val wresColl = for (wrec <- writableRecs) yield  {
					val wres = write(wrec, wc)
					wres
				}
				wresColl
			} else Nil
			mapOut(inMsg, writtenResults, pumpCtx)
		}
	}
	
	def getInMsgType : Class[_ >: InMsgType]
	def getOutMsgType : Class[_ <: OutMsgType]
	def getCtxType : Class[_ >: CtxType]
}
// Destinations are 
// 
// 
// https://twitter.github.io/scala_school/type-basics.html
// "A function’s return value type is covariant. If you need a function that returns a Bird but have a function 
// that returns a Chicken, that’s great.""
