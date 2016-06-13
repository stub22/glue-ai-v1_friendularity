package org.friendularity.respire

import java.io.{ByteArrayInputStream, StringReader}
import java.nio.charset.StandardCharsets

import akka.actor.{Actor, ActorContext, ActorRef}

import com.hp.hpl.jena.rdf.model.{Model => JenaModel, ModelFactory => JenaModelFactory, Resource, ResIterator}
import org.apache.jena.riot.RDFFormat
import org.appdapter.core.item.JenaResourceItem
import org.appdapter.core.name.{FreeIdent, Ident}
import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.api.thing.ThingActionSpec
import org.cogchar.impl.thing.basic.BasicThingActionSpec
import org.cogchar.name.dir.NamespaceDir
import org.cogchar.name.thing.ThingCN
import org.cogchar.render.goody.basic.BasicGoodyCtx
import org.friendularity.dull.ThingActExposer

import scala.collection.mutable.ListBuffer

/**
  * Created by Owner on 5/25/2016.
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

trait VWGoodyJobLogic extends VarargsLogging {
	protected def getGoodyCtx : BasicGoodyCtx
	protected def processVWGoodyRequest(vwmsg : VWorldRequest, slfActr : ActorRef, localActorCtx : ActorContext): Unit = {
		vwmsg match {

			case goodyActSpecMsg: VWGoodyRqActionSpec => processVWGoodyActSpec(goodyActSpecMsg, localActorCtx)

			case goodyRdfMsg: VWGoodyRqRdf => processVWGoodyRdfMsg(goodyRdfMsg, slfActr, localActorCtx)
		}
	}
	protected def processVWGoodyActSpec (goodyActSpecMsg : VWGoodyRqActionSpec, localActorCtx : ActorContext) : Unit = {
		val actSpec = goodyActSpecMsg.getActionSpec
		info4("VWGoodyJobLogic is processing received actSpec of class={}, verb={}, tgtType={} tgtID={}",
					actSpec.getClass, actSpec.getVerbID, actSpec.getTargetThingTypeID, actSpec.getTargetThingID)
		val goodyCtx = getGoodyCtx
		goodyCtx.consumeAction(actSpec)
	}

	protected def processVWGoodyRdfMsg (goodyMsg : VWGoodyRqRdf, slfActr : ActorRef, localActorCtx : ActorContext) : Unit = {
		val jenaModel = goodyMsg.asJenaModel(None)
		val exposer = new ThingActExposer {}
		val thingActs : List[ThingActionSpec] = exposer.extractThingActsFromModel(jenaModel)

		if (thingActs.isEmpty) {
			warn1("Found 0 ThingActs in inbound VWGoodyRqRdf message, dumping model:\n {}", jenaModel)
		}
		if (thingActs.length > 1) {
			warn1("Found {} ThingActs in inbound VWGoodyRqRdf, processing in arbitrary order (TODO: sort by timestamp)", thingActs.length : Integer)
		}

		for (tas <- thingActs) {
			val specMsg = new VWGoodyRqTAS(tas)
			slfActr.tell(specMsg, slfActr)
		}

	}
}
class VWGoodyActor(myGoodyCtx : BasicGoodyCtx) extends Actor with VWGoodyJobLogic {
	override protected def getGoodyCtx : BasicGoodyCtx = myGoodyCtx
	def receive = {
		case vwrq: VWorldRequest => {
			processVWGoodyRequest(vwrq, self, context)
		}
	}
}