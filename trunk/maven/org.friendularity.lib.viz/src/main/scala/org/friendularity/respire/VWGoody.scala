package org.friendularity.respire

import akka.actor.{Actor, ActorContext, ActorRef}
import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.impl.thing.basic.BasicThingActionSpec
import org.cogchar.render.goody.basic.BasicGoodyCtx

/**
  * Created by Owner on 5/25/2016.
  */
trait VWGoodyRqRdf extends VWContentRq  with RdfMsg {
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

	protected def processVWGoodyRdfMsg (goodyMsg : VWGoodyRqRdf, localActorCtx : ActorContext) : Unit = ???
}
class VWGoodyActor(myGoodyCtx : BasicGoodyCtx) extends Actor with VWGoodyJobLogic {
	override protected def getGoodyCtx : BasicGoodyCtx = myGoodyCtx
	def receive = {
		case vwrq: VWorldRequest => {
			processVWGoodyRequest(vwrq, self, context)
		}
	}
}