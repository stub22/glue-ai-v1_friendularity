package org.friendularity.akact

import akka.actor.{Actor, ActorLogging, ActorRef, Terminated}

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

/*
http://doc.akka.io/docs/akka/2.3.14/AkkaScala.pdf?_ga=1.62652207.2057866337.1455389997
p. 21

Creating Actors
An actor system is typically started by creating actors beneath the guardian actor using the
ActorSystem.actorOf method and then using ActorContext.actorOf from within the created
actors to spawn the actor tree. These methods return a reference to the newly created actor. Each actor has direct
access (through its ActorContext) to references for its parent, itself and its children. These references may be
sent within messages to other actors, enabling those to reply directly

Looking up Actors by Concrete Path
In addition, actor references may be looked up using the ActorSystem.actorSelection method. The
selection can be used for communicating with said actor and the actor corresponding to the selection is looked up
when delivering each message.
To acquire an ActorRef that is bound to the life-cycle of a specific actor you need to send a message, such as
the built-in Identify message, to the actor and use the sender() reference of a reply from the actor.


 */