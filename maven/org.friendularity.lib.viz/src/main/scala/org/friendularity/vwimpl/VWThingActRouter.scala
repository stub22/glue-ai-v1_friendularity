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

import akka.actor.{ActorRef, Props}
import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.api.thing.ThingActionSpec
import org.cogchar.render.app.entity.GoodyActionExtractor
import org.friendularity.akact.{KnowsAkkaSys, FrienduActor}
import org.friendularity.cpmsg.{ActorRefCPMsgTeller, CPStrongTeller}
import org.friendularity.vwmsg.{VWorldPublicTellers, VWRqTAWrapper}

/**
  * Created by Stub22 on 8/11/2016.
  */
trait VWThingActReqRouterLogic extends VarargsLogging {
	def routeRq(vwtarq: VWRqTAWrapper) : Unit = {
		val tellers = getVWPubTellers
		info1("Router handling TARq in binary format: {}, and wisely using vwpt-tellers...", vwtarq)

		val taSpec : ThingActionSpec = vwtarq.getActionSpec
		val gax: GoodyActionExtractor = new GoodyActionExtractor(taSpec)
		val targetTypeID = gax.getType
	}
	def getVWPubTellers : VWorldPublicTellers

	def handleGoodyTA(ta : ThingActionSpec) : Unit = {
		val goodyTeller = getVWPubTellers.getGoodyDirectTeller
	}
	def handleBodyTA(ta : ThingActionSpec) : Unit = {
		// Resolve body to Actor
	}
	def handleCameraTA(ta : ThingActionSpec) : Unit = {
		// Resolve cam to ID
	}

}
class VWThingActReqRouterActor(routerLogic : VWThingActReqRouterLogic) extends FrienduActor {
	override def receive = {
		case vwgrq: VWRqTAWrapper => {
			routerLogic.routeRq(vwgrq)
		}
		case other => {
			getLogger().warn("ThingActReqRouterActor received unexpected message: {}", other)
		}
	}
}
trait MakesVWTAReqRouterTeller extends KnowsAkkaSys with VarargsLogging {
	protected def makeBinSerRoutingTeller(vwpt : VWorldPublicTellers) : CPStrongTeller[VWRqTAWrapper] = {
		val akkaSys = getAkkaSys
		val routerLogic = new VWThingActReqRouterLogic {
			val myVWPT : VWorldPublicTellers = vwpt
			override def getVWPubTellers : VWorldPublicTellers = myVWPT
		}
		info1("Made routerLogic={}", routerLogic)
		val routerActorProps = Props(classOf[VWThingActReqRouterActor], routerLogic)
		val routerActorRef : ActorRef = akkaSys.actorOf(routerActorProps, "taBinSerRouterActr")
		info1("Made routerActorRef={}", routerActorRef)
		val routerTeller = new ActorRefCPMsgTeller[VWRqTAWrapper](routerActorRef)
		routerTeller
	}

}