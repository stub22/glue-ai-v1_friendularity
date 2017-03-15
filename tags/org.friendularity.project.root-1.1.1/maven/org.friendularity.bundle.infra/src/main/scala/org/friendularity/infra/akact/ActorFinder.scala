package org.friendularity.infra.akact

import akka.actor.{ActorRef, ActorSystem}

import scala.collection.mutable

/**
  * Created by Owner on 4/13/2016.
  */
trait TopActorFinder {
	def findTopActorRef(actorName : String) : ActorRef
}
trait CachingMakingTopActorFinder extends TopActorFinder {
	lazy val myTopActorRefsByName = new mutable.HashMap[String, ActorRef]()
	override def findTopActorRef(topActorName : String) : ActorRef = {
		myTopActorRefsByName.getOrElseUpdate(topActorName, makeTopActor(topActorName))
	}
	protected def makeTopActor(topActorName : String) : ActorRef
}
trait KnowsActorSystem {
	protected def getActorSys : ActorSystem
}
