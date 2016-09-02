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

import java.awt.Dimension

import akka.actor.{Actor, ActorContext, ActorRef}
import com.hp.hpl.jena.rdf.model.{Model => JenaModel, ModelFactory => JenaModelFactory}
import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.api.thing.ThingActionSpec
import org.cogchar.api.thing.WantsThingAction.ConsumpStatus
import org.cogchar.render.app.entity.{VWorldEntityReg, VWorldEntity, GoodyActionExtractor}
import org.cogchar.render.goody.basic.{BasicGoodyCtxImpl, BasicGoodyCtx}
import org.cogchar.render.sys.registry.RenderRegistryClient
import org.cogchar.render.sys.task.Queuer.QueueingStyle
import org.cogchar.render.sys.window.WindowStatusMonitor
import org.friendularity.thact.ThingActExposer
import org.friendularity.vwmsg.{VWRqTAWrapper, VWTARqRdf, VWRqTAWrapImpl, VWorldRequest}

/**
  * Created by Stub22 on 5/25/2016.
  *
  * Handles messages to (sometimes stateful) goody visual objects.
  * One instance of this logic may translate and forward messages for any number
  * of goody objects.
  *	These may involve:
  * create/destroy, attach/detach, movement (smooth or abrupt),
  * goody-state changes, color+transparency changes.
  *
  * Note that currently we don't support any outbound replies or notices from the Goody system.
  *
  * Messages to avatars/bodies and cameras are *not* handled here - they are processed upstream
  * of here in VWThingActRouter.
  *
  * Old BasicGoodyCtx defines these basic capabilities

    RenderRegistryClient getRRC();
    VWorldEntityReg getVWER();  -- defines addGoody, removeGoody, getGoody, hasGoodyAt, getAllGoodies
    Dimension getScreenDimension();
    void applyNewScreenDimension(Dimension var1);
    ConsumpStatus consumeAction(ThingActionSpec var1);

  While old VWorldEntity defines:

    public Ident getUri()

    public abstract void setPosition(Vector3f var1, QueueingStyle var2);
    public void setRotation(Quaternion newRotation, QueueingStyle style)
    public void setVectorScale(Vector3f scaleVector, QueueingStyle style)
    public void setUniformScaleFactor(Float scale, QueueingStyle style)

    public abstract void applyAction(GoodyActionExtractor var1, QueueingStyle var2);
    public void attachToVirtualWorldNode(Node attachmentNode, QueueingStyle style)
    public void detachFromVirtualWorldNode(QueueingStyle style)
    public void applyScreenDimension(Dimension screenDimension)

  */

trait VWGoodyJobLogic extends VarargsLogging {
	protected def getGoodyCtx : BasicGoodyCtx
	protected def processVWGoodyRequest(vwmsg : VWorldRequest, slfActr : ActorRef, localActorCtx : ActorContext): Unit = {
		vwmsg match {

			case taBinWrapMsg: VWRqTAWrapper => processVWGoodyActSpec(taBinWrapMsg, localActorCtx)

			case goodyRdfMsg: VWTARqRdf => processVWGoodyRdfMsg(goodyRdfMsg, slfActr, localActorCtx)
		}
	}
	protected def processVWGoodyActSpec (thingActSpecMsg : VWRqTAWrapper, localActorCtx : ActorContext) : Unit = {
		val actSpec = thingActSpecMsg.getActionSpec
		info4("VWGoodyJobLogic is processing received actSpec of class={}, verb={}, tgtType={} tgtID={}",
					actSpec.getClass, actSpec.getVerbID, actSpec.getTargetThingTypeID, actSpec.getTargetThingID)
		val goodyCtx = getGoodyCtx

		goodyCtx.consumeAction(actSpec)
	}

	protected def processVWGoodyRdfMsg (goodyMsg : VWTARqRdf, slfActr : ActorRef, localActorCtx : ActorContext) : Unit = {
		val jenaModel = goodyMsg.asJenaModel(None)
		val exposer = new ThingActExposer {}
		val thingActs : List[ThingActionSpec] = exposer.extractThingActsFromModel(jenaModel)

		if (thingActs.isEmpty) {
			warn1("Found 0 ThingActs in inbound VWGoodyRqRdf message, dumping model:\n {}", jenaModel)
		}
		if (thingActs.length > 1) {
			warn1("Found {} ThingActs in inbound VWGoodyRqRdf, processing in arbitrary order (TODO: sort by timestamp)", thingActs.length : Integer)
		}
		// Process all thingActs, immediately in this thread, in any order.
		for (tas <- thingActs) {

			val specMsg = new VWRqTAWrapImpl(tas)

			processVWGoodyActSpec(specMsg, localActorCtx)
			// We could instead requeue, as we used to do (summer 2016), like so:
			// 	slfActr.tell(specMsg, slfActr)
			// but that could change order of mixed TA processing from client's point of view.
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

class BetterBGC(rrc : RenderRegistryClient, winStatMon : WindowStatusMonitor) extends BasicGoodyCtxImpl(rrc, winStatMon) {
	override def applyNewScreenDimension(newDimension: Dimension): Unit = {
		super.applyNewScreenDimension(newDimension)
	}

	override def consumeAction(actionSpec: ThingActionSpec): ConsumpStatus = {
		super.consumeAction(actionSpec)
	}

	override def getVWER: VWorldEntityReg = {
		super.getVWER
	}

	override def createAndAttachByAction(ga: GoodyActionExtractor, qStyle: QueueingStyle): VWorldEntity = {
		super.createAndAttachByAction(ga, qStyle)
	}
}