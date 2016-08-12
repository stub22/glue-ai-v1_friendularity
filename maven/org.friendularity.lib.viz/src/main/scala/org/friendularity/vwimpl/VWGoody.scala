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

package org.friendularity.vwimpl

import akka.actor.{Actor, ActorContext, ActorRef}
import com.hp.hpl.jena.rdf.model.{Model => JenaModel, ModelFactory => JenaModelFactory}
import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.api.thing.ThingActionSpec
import org.cogchar.render.goody.basic.BasicGoodyCtx
import org.friendularity.thact.ThingActExposer
import org.friendularity.vwmsg.{VWGoodyRqTAWrapper, VWGoodyRqRdf, VWGoodyRqTAS, VWorldRequest}

/**
  * Created by Stub22 on 5/25/2016.
  */

trait VWGoodyJobLogic extends VarargsLogging {
	protected def getGoodyCtx : BasicGoodyCtx
	protected def processVWGoodyRequest(vwmsg : VWorldRequest, slfActr : ActorRef, localActorCtx : ActorContext): Unit = {
		vwmsg match {

			case goodyActSpecMsg: VWGoodyRqTAWrapper => processVWGoodyActSpec(goodyActSpecMsg, localActorCtx)

			case goodyRdfMsg: VWGoodyRqRdf => processVWGoodyRdfMsg(goodyRdfMsg, slfActr, localActorCtx)
		}
	}
	protected def processVWGoodyActSpec (goodyActSpecMsg : VWGoodyRqTAWrapper, localActorCtx : ActorContext) : Unit = {
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

trait MoveBodiesLogic {

}
trait MoveCamerasLogic {

}
trait PublishStatsLogic {

}