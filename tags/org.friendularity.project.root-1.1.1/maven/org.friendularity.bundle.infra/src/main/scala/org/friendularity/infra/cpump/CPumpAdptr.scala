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

import org.friendularity.infra.cpmsg.CPumpMsg

trait WritingCtx 

trait WritableRecord

trait WrittenResult {
	
}
// Note that an Adptr knows how to use a supplied ctx method arg, but it may not keep its own ctx ref.
// Adptrs should be stateless (immutable) and threadsafe, except for the effects of the write method, but those
// write effects should all be routed through supplied argument wc : WritingCtx.
//
// With these constraints, we can rely on CPumpAdptr to be java-Serializable, and thus usable
// as arguments to construct a remote chan (listener).
// 
// Adptr design is akin to monad pipeline:  See the flow in processMsg.
// More refined impls can use actor msgs to enqueue each step.
// Alternatively, those steps can be map-reduced by a fabric such as hadoop, spark, etc.
// 
// To process a specific kind of input message, one should extend this trait directly.
// Note that we also  allow for a further amount of data-switched type narrowing via the
// getUsualInMsgType method.
//
trait CPumpAdptr[-InMsgType <: CPumpMsg, -CtxType <: CPumpCtx, +OutMsgType <: CPumpMsg] extends java.io.Serializable {

	// Nonempty result collection => shortcut succeeded
	// Argument (or precursor of) inMsg is known to have passed the maybeInterested test, before this is called.
	protected def	attemptShortcut(inMsg : InMsgType, pumpCtx_opt : Option[CtxType]) : Traversable[OutMsgType]
	
	// Shortcut must have failed, so full input decoding step is next.
	protected def mapIn(inMsg : InMsgType, pumpCtx_opt : Option[CtxType]) : Traversable[WritableRecord]
	
	// There could be a delay between call to mapIn and call to write, and that could cause problems.
	// TODO:  Remove the problems, the delay should be tolerated (from robustness perspective).

	// WritingCtx provides required destination handle.
	// WrittenResult supplies everything the downstream explicitly knows about the operation.
	// Future steps after this cannot inspect rec.
	protected def write(rec : WritableRecord, wc : WritingCtx) : WrittenResult // Stateful activity limited to here
	
	// There may be a delay between the call to write and the call to mapOut.
	// mapOut does not get to see the WritableRecord, only the input and the WrittenResult.
	protected def mapOut(inMsg : InMsgType, wresults : Traversable[WrittenResult], pumpCtx_opt : Option[CtxType]) : Traversable[OutMsgType]

	// Deeper impls of processMsg should be elaborations of this same workflow, but use more queueing.
	// There, each of the steps below will be taken in response to a separate actor message.
	def processMsg(inMsg : InMsgType, pumpCtx_opt : Option[CtxType]) : Traversable[OutMsgType] = {
		
		val shortcutResults = attemptShortcut(inMsg, pumpCtx_opt)
		if (shortcutResults.nonEmpty) {
			shortcutResults
		} else {
			val writableRecs = mapIn(inMsg, pumpCtx_opt)
			val writtenResults : Traversable[WrittenResult] = if (writableRecs.nonEmpty) {
				val wc : WritingCtx = null
				val wresColl = for (wrec <- writableRecs) yield  {
					val wres = write(wrec, wc)
					wres
				}
				wresColl
			} else Nil
			mapOut(inMsg, writtenResults, pumpCtx_opt)
		}
	}
	
	// "Legal" runtime type queries
	// Type boundsare  defined in compliance with variance annotations on this trait.
	// That means 1) We can cleanly inheritance-type-specialize our adopters on one 
	// important axis: by output msg type.
	 
	def getLegalOutMsgClz : Class[_ <: OutMsgType]
	
	// 2) But We cannot really inheritance-type-specialize over InMsgType or CtxType, except
	// by binding the trait type parameters on each subtype binding.
	// 
	// Thus these next two methodsare not highly expressive, but can be crudely useful, occasionally.
	// Somewhat useful to verify context assumptions from outside; could support some auto re-writing.
	
	def getLegalInMsgClz : Class[_ >: InMsgType] // Usually just returns InMsgType.
	
	def getLegalCtxClz : Class[_ >: CtxType] // Usually just returns CtxType.
	
	// Finally, we have this extra accessor to support user narrowing of input semantic-type through simple inheritance.
	// For clz.data-matching, not java type-matching.  Typebound is in covariant dir.
	// Usage:  Suppose we have a FruitPump[FOut extends FruitOut] extends CPumpAdptr[Fruit,FCtx,FOut].  
	// (Now FruitPump is a one-parameter type, whereas CPumpAdptr is a 3 parameter type)
	// Then suppose Apple extends Fruit, and AppleCoresAndPeels extends FruitOut.
	// Then  we can have  ApplePump extends FruitPump[AppleCoresAndPeels] and overrides   getUsualInMsgType = classOf[Apple].
	// All the working methods of ApplePump must still accept input messages of any kind of fruit, type-wise, 
	// but semantically they can auto-exclude non-apples, possibly helped by this method.
	
	def getUsualInMsgClz : Class[_ >: InMsgType]  // Could return any subtype or supertype of InMsgType

	// Can be used internally in subtypes, or from listenChan.interested
	def matchesUsualInClz (msg : AnyRef) : Boolean = {
		val usualInType = getUsualInMsgClz
		usualInType.isInstance(msg)
	}
	// First line of defense, weakest condition.  Used for channel routing.
	// Subclasses may override if they think they maybeInterested in input outside their usual type.
	// Subclasses in principle could also supply a narrower type as a logical partition, but that would be a bad idea,
	// since this method is just the *first* check of interest for the adopter.
	def maybeInterested(inMsg : AnyRef) : Boolean = matchesUsualInClz(inMsg)

}
// Destinations are 
// 
// 
// https://twitter.github.io/scala_school/type-basics.html
// "A function’s return value type is covariant. If you need a function that returns a Bird but have a function 
// that returns a Chicken, that’s great.""
