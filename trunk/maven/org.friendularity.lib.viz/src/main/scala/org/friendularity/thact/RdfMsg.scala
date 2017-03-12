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

package org.friendularity.thact

import java.io.ByteArrayInputStream
import java.lang.{Long => JLong}
import java.nio.charset.StandardCharsets

import com.hp.hpl.jena.rdf.model.{Model => JenaModel, ModelFactory => JenaModelFactory, Resource, ResIterator}

import org.apache.jena.riot.RDFFormat
import org.appdapter.fancy.log.VarargsLogging
import org.friendularity.infra.cpmsg.{CPMsgTeller, CPSymbolMsg, CPRepliableMsg}

/**
  *
  */
// Just blobs of standalone RDF models.   Not used with query/update languages, at this time.

trait RdfMsg {
	def asTurtleString : String

	def asJenaModel(flags_opt: Option[AnyRef]) : JenaModel
	// def asR2goModel : AnyRef
}
trait RdfMsgIntrp extends VarargsLogging {
	def rcvMsg(rdfMsg: RdfMsg) = {
		val rcvdTrtlTxt = rdfMsg.asTurtleString
		debug1("Received turtle-txt msg txt, len={}", rcvdTrtlTxt.length: Integer)
		val rcvdJenm = rdfMsg.asJenaModel(None)
		info1("Received and parsed msg to model of at least {} stmt-triples.", rcvdJenm.size() : JLong)

	}
}

import org.cogchar.api.thing.ThingActionSpec

trait CPThingActionMsg extends CPSymbolMsg {
	def getThingAction : ThingActionSpec
}

trait  CPRepliableThingActionMsg extends CPThingActionMsg with CPRepliableMsg

case class CPTAWrapMsg(mySerialTASpec : ThingActionSpec, myReplyTeller_opt : Option[CPMsgTeller])
			extends CPRepliableThingActionMsg {
	override def getThingAction : ThingActionSpec = mySerialTASpec
	override def getReplyTeller_opt : Option[CPMsgTeller] = myReplyTeller_opt
}
