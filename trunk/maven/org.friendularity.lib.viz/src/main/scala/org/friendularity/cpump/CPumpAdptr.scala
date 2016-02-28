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
// 
// To process a specific kind of input message, one should extend this trait directly.
// Note that we do allow for a further amount of data-switched type narrowing via the 
// getUsualInMsgType method.
// Because InMsgType is a contravariant parameter.
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
	
	// Deeper impls of processMsg should be elaborations of this same workflow, but use more queueing.
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
	
	// "Legal" runtime type queries
	// Type boundsare  defined in compliance with variance annotations on this trait.
	// That means 1) We can cleanly inheritance-type-specialize our adopters on one 
	// important axis: by output msg type.
	 
	def getLegalOutMsgType : Class[_ <: OutMsgType]
	
	// 2) But We cannot really inheritance-type-specialize over InMsgType or CtxType, except
	// by binding the trait type parameters on each subtype binding.
	// 
	// Thus these next two methodsare not highly expressive, but can be crudely useful, occasionally.
	// Somewhat useful to verify context assumptions from outside; could support some auto re-writing.
	
	def getLegalInMsgType : Class[_ >: InMsgType] // Usually just returns InMsgType.  
	
	def getLegalCtxType : Class[_ >: CtxType] // Usually just returns CtxType.
	
	// Finally, we have this extra accessor to support user narrowing of input semantic-type through simple inheritance.
	// For clz.data-matching, not java type-matching.  Typebound is in covariant dir.
	// Usage:  Suppose we have a FruitPump[FOut extends FruitOut] extends CPumpAdptr[Fruit,FCtx,FOut].  
	// (Now FruitPump is a one-parameter type, whereas CPumpAdptr is a 3 parameter type)
	// Then suppose Apple extends Fruit, and AppleCoresAndPeels extends FruitOut.
	// Then  we can have  ApplePump extends FruitPump[AppleCoresAndPeels] and overrides   getUsualInMsgType = classOf[Apple].
	// All the working methods of ApplePump must still accept input messages of any kind of fruit, type-wise, 
	// but semantically they can auto-exclude non-apples, possibly helped by this method.
	
	def getUsualInMsgType : Class[_ >: InMsgType]  // Could return any subtype or supertype of InMsgType
	
	// Can be used internally in subtypes, or from listenChan.interested
	def matchesUsualInType (msg : InMsgType) : Boolean = {
		val usualInType = getUsualInMsgType
		usualInType.isInstance(msg)
	}
}
// Destinations are 
// 
// 
// https://twitter.github.io/scala_school/type-basics.html
// "A function’s return value type is covariant. If you need a function that returns a Bird but have a function 
// that returns a Chicken, that’s great.""
