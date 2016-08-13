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
import java.lang
import java.lang.{Double, Float, Boolean}
import java.nio.charset.StandardCharsets


import com.hp.hpl.jena.rdf.model.{Model => JenaModel, ModelFactory => JenaModelFactory, Resource, ResIterator}
import org.apache.jena.riot.RDFFormat
import org.appdapter.core.name.Ident
import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.api.thing.{SerTypedValueMap, TypedValueMap, ActionParamWriter, ThingActionSpec}
import org.cogchar.api.vworld.GoodyActionParamWriter
import org.cogchar.impl.thing.basic.{BasicTypedValueMap, BasicThingActionSpec}
import org.cogchar.impl.thing.fancy.ConcreteTVM
import org.friendularity.thact.{JenaModelReader, RdfMsg}
import org.friendularity.vwimpl.IdentHlp


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

trait MainFloatParams {

}
trait VWTAMsgMaker extends IdentHlp {
	val myAgentID : Ident = makeStampyRandyIdentAnon()

	def makeTASpec(entityID : Ident, typeID : Ident, verbID : Ident, paramSerMap: SerTypedValueMap) : ThingActionSpec = {
		val actRecID : Ident = makeStampyRandyIdent("actSpec")
		// gar.writeToMap(paramWriter)
		val srcAgentID: Ident = myAgentID
		val postedTStampMsec: Long = System.currentTimeMillis
		val actionSpec = new BasicThingActionSpec(actRecID, entityID, typeID, verbID, srcAgentID, paramSerMap, postedTStampMsec)
		actionSpec
	}
	def writePos(gapw : GoodyActionParamWriter, maybeLocated3D: MaybeLocated3D) : Unit = {
		if (maybeLocated3D.getPos_opt.isDefined) {
			val pos = maybeLocated3D.getPos
			gapw.putLocation(pos.getX, pos.getY, pos.getZ)
		}
	}
	def writeRot(gapw : GoodyActionParamWriter, maybeRot: MaybeRotated3D) : Unit = {

	}
	def writeScale(gapw : GoodyActionParamWriter, maybeScale: MaybeScaled3D) : Unit = {
		if (maybeScale.getScl_opt.isDefined) {
			val scl = maybeScale.getScale
			gapw.putScaleVec(scl.getX, scl.getY, scl.getZ)
		}
	}
	def writeXform3D(gapw : GoodyActionParamWriter, mayXform : MaybeTransform3D) : Unit = {
		writePos(gapw, mayXform)
		writeRot(gapw, mayXform)
		writeScale(gapw, mayXform)
	}
	def writeXform3D(paramSerMap: SerTypedValueMap, mayXform : MaybeTransform3D) : Unit = {
		val gapw = new GoodyActionParamWriter(paramSerMap)
		writeXform3D(gapw, mayXform)
	}
}
