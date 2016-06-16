package org.friendularity.dull


import org.appdapter.fancy.log.VarargsLogging
import com.hp.hpl.jena.rdf.model.{Model => JenaModel}
import java.lang.{Long => JLong}

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
