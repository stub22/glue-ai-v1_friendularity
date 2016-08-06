package org.friendularity.akact

import akka.actor.{Props, ActorRef, ActorRefFactory, Actor}
import org.appdapter.fancy.log.VarargsLogging

/**
  * Created by Owner on 8/6/2016.
  */
abstract class FrienduActor() extends Actor with VarargsLogging
// Seems that using "with ActorLogging" here leads
class DummyDumpingActor(argGoesHere : String) extends FrienduActor {
	getLogger.info("In dummy actor constructor, arg={}", argGoesHere)
	def receive = {
		case msg: AnyRef => {
			val msgDump = msg.toString()
			getLogger.info("DummyDumpingActor received msg of clazz={} and dump-len={}", msg.getClass, msgDump.length)
			getLogger.debug("Received message dump:\n{}", msgDump)
		}
	}
}

trait DummyActorMaker extends VarargsLogging {
	def makeTestDummyActor(parentARF : ActorRefFactory, dummyActorName : String) : ActorRef = {
		val argInstruct = """This constructor arg could be any java object,
				but should be java-serializable if used in dynamic network context (see akka docs)."""
		val dummyActorProps = Props(classOf[DummyDumpingActor], argInstruct)
		val dummyActorRef : ActorRef = parentARF.actorOf(dummyActorProps, dummyActorName)
		dummyActorRef
	}

}

