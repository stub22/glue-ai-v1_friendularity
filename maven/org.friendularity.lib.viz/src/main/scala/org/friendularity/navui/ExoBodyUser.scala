package org.friendularity.navui

import akka.actor.{Props, ActorRef, ActorRefFactory, Actor}
import org.appdapter.fancy.log.VarargsLogging
import org.friendularity.respire.VWBodyNotice

/**
  * Created by Owner on 6/8/2016.
  */
// Beyond all the Outer stuff, we have:

class ExoBodyUserLogic extends VarargsLogging {
	def rcvBodyNotice(bodyNotice : VWBodyNotice): Unit = {
		info1("ExoBody UserLogic received bodyNotice={}", bodyNotice)
	}
}

class ExoBodyUserActor(bodyUserLogic : ExoBodyUserLogic)  extends Actor {
	def receive = {
		case vwbn: VWBodyNotice => bodyUserLogic.rcvBodyNotice(vwbn)

	}
}
object ExoActorFactory {
	def makeExoBodyUserActor(parentARF : ActorRefFactory, ebuActorName : String, userLogic : ExoBodyUserLogic) : ActorRef = {
		val ebuActorProps = Props(classOf[ExoBodyUserActor], userLogic)
		val ebuActorRef : ActorRef = parentARF.actorOf(ebuActorProps, ebuActorName)
		ebuActorRef
	}

}