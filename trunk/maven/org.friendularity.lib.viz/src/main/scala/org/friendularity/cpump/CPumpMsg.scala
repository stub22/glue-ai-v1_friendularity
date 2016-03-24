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

import org.appdapter.core.name.Ident

// Empty (so far) marker trait for all msgs.
trait CPumpMsg extends java.io.Serializable {

}

// Contains regular-shaped buffer streams of data as opaque binary or text
trait CPSignalMsg extends CPumpMsg {
	
}

// Contains graph metadata, as text or tuples
trait CPSymbolMsg extends CPumpMsg {
	
}
case class TxtSymMsg(mySymTxt : String) extends CPSymbolMsg {

}

trait CPAdminRequestMsg[CtxBound <: CPumpCtx] extends CPumpMsg {
	def processInCtx(ctx : CtxBound)
}
case class CPARM_RegDullListenChan[LMK <: CPumpMsg](chanID : Ident, listenedMsgClz : Class[LMK],
					  adoptrs : Traversable[CPumpAdptr[LMK, DullPumpCtx, CPumpMsg]])
					extends CPAdminRequestMsg[DullPumpCtx] {
	override def processInCtx(ctx : DullPumpCtx): Unit = {
		val listenChan = ctx.makeOnewayListenChan(chanID, listenedMsgClz, adoptrs)
	}
}


