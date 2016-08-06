package org.friendularity.thact

import java.lang.{Long => JLong}

import com.hp.hpl.jena.rdf.model.{Model => JenaModel}
import org.appdapter.fancy.log.VarargsLogging
import org.friendularity.cpmsg.{CPMsgTeller, CPSymbolMsg, CPRepliableMsg}

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
