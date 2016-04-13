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

import akka.actor.{ActorRef, ActorLogging, Actor}
import org.appdapter.core.name.{FreeIdent, Ident}
import org.appdapter.fancy.log.VarargsLogging;

trait CPumpListChanFinder[CtxType <: CPumpCtx] {
	// The PumpCtx is responsible for mapping any input msg to a set of possible listeners.
	// Many/all of the listers may actually choose to ignore the message, and it is acceptable
	// (if not always efficient) for Ctx to return all channels, if it has no heuristic filter.
	def findMsgListenChans[MK <: CPumpMsg](postChan : CPChanPost[MK, CtxType], postedMsg : MK) : Traversable[CPChanListen[MK, CtxType]]

}
trait CPumpCtx {
	// PostChans are mainly just for bookeeping and reply/receipt routing, but a pumpCtx can also append 
	// other tracking state as needed.

// 	def postAndForget[MK <: CPumpMsg](postChan : CPChanPost[MK], postedMsg : MK) : Unit = {
		// enq for mapping, or map then enq for each listener
		// forget=> we don't care about receiving or tracking any of the results (immediate or eventual).
	// }

}
// Use of this trait implies actor awareness
trait BoundedCPumpCtx {
	def getBoundaryActorRef_opt : Option[ActorRef] = None
}
// Use of this trait means the Ctx will get its boundary actor injected sometime *after* Ctx construction.
trait MutaBoundedCPumpCtx extends BoundedCPumpCtx with VarargsLogging {

	private var myBoundaryAR_opt : Option[ActorRef] = None
	override def getBoundaryActorRef_opt : Option[ActorRef] = myBoundaryAR_opt

	def setBoundaryActorRef(aref : ActorRef): Unit = {
		if (myBoundaryAR_opt.isDefined) {
			warn3("Boundary ref for {} changing from {} to {}", this, myBoundaryAR_opt.get, aref)
		}
		myBoundaryAR_opt = Option(aref)
	}
}