package org.friendularity.vwmsg

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets


import com.hp.hpl.jena.rdf.model.{Model => JenaModel, ModelFactory => JenaModelFactory, Resource, ResIterator}
import org.apache.jena.riot.RDFFormat
import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.api.thing.ThingActionSpec
import org.friendularity.thact.RdfMsg


/**
  * Created by Owner on 6/16/2016.
  */
trait VWGoodyRqRdf extends VWContentRq  with RdfMsg {
}
case class VWGoodyRqTurtle(myTurtleTxt : String) extends VWGoodyRqRdf with VarargsLogging{
	override def asTurtleString : String = myTurtleTxt

	override def asJenaModel(flags_opt: Option[AnyRef]) : JenaModel = {
		val modelTurtleTxt : String = asTurtleString
		// val modelReader = new StringReader(modelTurtleTxt)
		val modelBytes = modelTurtleTxt.getBytes(StandardCharsets.UTF_8)
		val modelByteStream = new ByteArrayInputStream(modelBytes);
		val model = JenaModelFactory.createDefaultModel() ;
		val baseURI_orNull : String = null
		val lang : String = RDFFormat.TURTLE.getLang.getName //  "TURTLE"
		model.read(modelByteStream, baseURI_orNull, lang)
		info1("After read, model size is (at least) {} stmts", model.size() : java.lang.Long)
		debug1("Model contentDump:\n{}", model)
		model
	}
}
trait VWGoodyRqActionSpec extends VWContentRq {
	def getActionSpec : ThingActionSpec
}
case class VWGoodyRqTAS(myBTAS : ThingActionSpec) extends  VWGoodyRqActionSpec {
	override def getActionSpec : ThingActionSpec = myBTAS
}
