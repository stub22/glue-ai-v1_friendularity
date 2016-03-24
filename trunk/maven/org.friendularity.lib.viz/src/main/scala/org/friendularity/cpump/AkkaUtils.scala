package org.friendularity.cpump

import akka.actor.{Terminated, ActorLogging, Actor, ActorRef}

/**
  * Created by Owner on 3/24/2016.
  */
object AkkaUtils {

}

// This Actor watches the ref, and when it terminates, this actor sends .shutdown to the context actorSystem.
class AkkaSysTerminator(ref: ActorRef) extends Actor with ActorLogging {

	// "watch" registers us for lifecycle events on ref
	context watch ref
	def receive = {
		case Terminated(_) => {
			log.info("{} has terminated, so now we will down actor system", ref.path)
			context.system.shutdown()
		}
	}
}
