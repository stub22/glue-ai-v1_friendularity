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

package org.friendularity.vwmsg

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets


import com.hp.hpl.jena.rdf.model.{Model => JenaModel, ModelFactory => JenaModelFactory, Resource, ResIterator}
import org.apache.jena.riot.RDFFormat
import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.api.thing.ThingActionSpec
import org.friendularity.thact.{JenaModelReader, RdfMsg}


/**
  * Created by Stub22 on 6/16/2016.
  */


trait VWTARqRdf extends VWContentRq  with RdfMsg {
}
case class VWTARqTurtle(myTurtleTxt : String) extends VWTARqRdf with JenaModelReader with VarargsLogging{
	override def asTurtleString : String = myTurtleTxt

	override def asJenaModel(flags_opt: Option[AnyRef]) : JenaModel = {
		val modelTurtleTxt : String = asTurtleString
		readModelFromTurtleTxt(modelTurtleTxt, flags_opt)
	}
}
trait VWRqTAWrapper extends VWContentRq {
	def getActionSpec : ThingActionSpec
}
// trait VWGoodyRqTAWrapper extends VWRqTAWrapper

case class VWRqTAWrapImpl(myBTAS : ThingActionSpec) extends  VWRqTAWrapper {
	override def getActionSpec : ThingActionSpec = myBTAS
}


trait VWStatMsg extends VWorldNotice


