package org.friendularity.dull

import akka.actor.ActorSystem
import org.appdapter.fancy.log.VarargsLogging
import org.friendularity.cpump.{BoundaryTellerFinder, MutaBoundedCPumpCtxImpl}

import scala.collection.mutable

/**
  * Created by Owner on 4/13/2016.
  */

// Segregated => Each DullPumpCtx corresponds to exactly one topActor, which is the boundary for that Ctx.
// Chans within that Ctx will see that boundary=top actor as their internal admin contact.
// Outer-teller actors for each channel will be direct *children" of that boundary=top actor.
class SegregatedBoundedDullPumpCtx extends DullPumpCtx with MutaBoundedCPumpCtxImpl {
	override def getBoundaryTellerFinder_opt : Option[BoundaryTellerFinder] = Some(this)
}

// Keeps an explicit map so we can re-find a context in case of actor replacement.
class SegregatedDullCtxMgr extends KnowsDullPumpCtx with VarargsLogging {
	lazy val myDullPumpCtxsByTopActorName = new mutable.HashMap[String, DullPumpCtx]
	override protected def findDullPumpCtx(topActorName : String) : DullPumpCtx = {

		myDullPumpCtxsByTopActorName.getOrElseUpdate(topActorName, makeDullPumpCtx(topActorName))
	}
	// Override this to make app-specific DullPumpCtx.
	protected def makeDullPumpCtx(topActorName : String) : DullPumpCtx = {
		warn1("Making a nonspecific SegregatedBoundedDullPumpCtx for topActor={}, but usually this method gets overridden!", topActorName)
		new SegregatedBoundedDullPumpCtx()
	}
}

class SegregatedDullPumpSpace(private val myAkkaSys : ActorSystem) extends SegregatedDullCtxMgr with  DullTopActorFinder {

	protected def getActorSys : ActorSystem = myAkkaSys

}
