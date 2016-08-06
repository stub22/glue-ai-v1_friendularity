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

package org.friendularity.cpmsg

import java.io.{Serializable => JSerializable}

import akka.actor.ActorContext
import org.appdapter.core.name.Ident

// Empty (so far) marker trait for all msgs.
trait CPumpMsg extends JSerializable {

}

trait CPRepliableMsg extends CPumpMsg {
	def getReplyTeller_opt : Option[CPMsgTeller] = None
}

// Contains regular-shaped buffer streams of data as opaque binary or text
trait CPSignalMsg extends CPumpMsg {

}

// Contains graph metadata, as text or tuples, and possibly system wiring info.
trait CPSymbolMsg extends CPumpMsg {

}

// Q:  How well does logging play with serializable?



case class TxtSymMsg(mySymTxt : String) extends CPSymbolMsg {

}
case class RepliableTxtSymMsg(mySymTxt : String, myReplyTeller_opt : Option[CPMsgTeller])
			extends CPSymbolMsg with CPRepliableMsg {
	override def getReplyTeller_opt : Option[CPMsgTeller] = myReplyTeller_opt

}

// teller_opt eq None => "The channel was found, but we cannot give you a direct teller to it"
case class FoundOuterTellerMsg(chanID : Ident, outerTeller_opt : Option[CPMsgTeller]) extends CPumpMsg

// teller_opt eq None => "The channel was created, but we cannot give you a direct teller to it"
case class CreatedChanTellerMsg(chanID : Ident, createdTeller_opt : Option[CPMsgTeller]) extends CPumpMsg {
	def getConfirmedTeller : Option[CPMsgTeller] = createdTeller_opt
}
