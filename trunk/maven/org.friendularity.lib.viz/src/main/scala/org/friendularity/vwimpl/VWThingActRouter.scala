package org.friendularity.vwimpl

import akka.actor.{ActorRef, Props}
import org.appdapter.fancy.log.VarargsLogging
import org.friendularity.akact.{KnowsAkkaSys, FrienduActor}
import org.friendularity.cpmsg.{ActorRefCPMsgTeller, CPStrongTeller}
import org.friendularity.vwmsg.{VWGoodyRqTAWrapper, VWorldPublicTellers, VWRqTAWrapper}

/**
  * Created by Owner on 8/11/2016.
  */
trait VWThingActReqRouterLogic extends VarargsLogging {
	def routeRq(vwgrq: VWRqTAWrapper) : Unit = {
		val tellers = getVWPubTellers
		info1("Router handling TARq in binary format: {}, and wisely using vwpt-tellers...", vwgrq)
	}
	def getVWPubTellers : VWorldPublicTellers
}
class VWThingActReqRouterActor(routerLogic : VWThingActReqRouterLogic) extends FrienduActor {
	override def receive = {
		case vwgrq: VWGoodyRqTAWrapper => {
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