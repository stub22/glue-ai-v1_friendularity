package org.friendularity.respire

import java.io.StringReader

import akka.actor.{Actor, ActorContext, ActorRef}

import com.hp.hpl.jena.rdf.model.{Model => JenaModel, ModelFactory => JenaModelFactory}
import org.appdapter.core.name.{FreeIdent, Ident}
import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.impl.thing.basic.BasicThingActionSpec
import org.cogchar.render.goody.basic.BasicGoodyCtx
import org.friendularity.dull.ThingActExposer

/**
  * Created by Owner on 5/25/2016.
  */
trait VWGoodyRqRdf extends VWContentRq  with RdfMsg {
}
case class VWGoodyRqTurtle(myTurtleTxt : String) extends VWGoodyRqRdf with VarargsLogging{
	override def asTurtleString : String = myTurtleTxt

	override def asJenaModel(flags_opt: Option[AnyRef]) : JenaModel = {
		val modelTurtleTxt : String = asTurtleString
		val modelReader = new StringReader(modelTurtleTxt)
		val model = JenaModelFactory.createDefaultModel() ;
		val baseURI_orNull : String = null
		val lang : String = "TURTLE"
		model.read(modelReader, baseURI_orNull, lang)
		info1("After read, model size is (at least) {} stmts", model.size() : java.lang.Long)
		debug1("Model contentDump:\n{}", model)
		model
	}
}
trait VWGoodyRqActionSpec extends VWContentRq {
	def getActionSpec : BasicThingActionSpec
}
case class VWGoodyRqBTAS(myBTAS : BasicThingActionSpec) extends  VWGoodyRqActionSpec {
	override def getActionSpec : BasicThingActionSpec = myBTAS
}

trait VWGoodyJobLogic extends VarargsLogging {
	protected def getGoodyCtx : BasicGoodyCtx
	protected def processVWGoodyRequest(vwmsg : VWorldRequest, slfActr : ActorRef, localActorCtx : ActorContext): Unit = {
		vwmsg match {

			case goodyActSpecMsg: VWGoodyRqActionSpec => processVWGoodyActSpec(goodyActSpecMsg, localActorCtx)

			case goodyRdfMsg: VWGoodyRqRdf => processVWGoodyRdfMsg(goodyRdfMsg, localActorCtx)
		}
	}
	protected def processVWGoodyActSpec (goodyActSpecMsg : VWGoodyRqActionSpec, localActorCtx : ActorContext) : Unit = {
		val actSpec = goodyActSpecMsg.getActionSpec
		info1("VWBossLogic is processing received actSpec: {}", actSpec)
		val goodyCtx = getGoodyCtx
		goodyCtx.consumeAction(actSpec)
	}

	protected def processVWGoodyRdfMsg (goodyMsg : VWGoodyRqRdf, localActorCtx : ActorContext) : Unit = {
		val jenaModel = goodyMsg.asJenaModel(None)

		// TODO:  Find all URIs of ThingActions, using rdf:type
		//  @prefix ccrt:  <urn:ftd:cogchar.org:2012:runtime#> .
		//   ccrt:ThingAction ;


		val exposer = new ThingActExposer {}

		// val actionID : Ident = new FreeIdent("wrong_so_fix_me#id")
		// val actSpec = exposer.readActionFromJenaModel(jenaModel, actionID)

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