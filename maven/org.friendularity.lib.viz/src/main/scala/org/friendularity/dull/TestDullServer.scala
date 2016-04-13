package org.friendularity.dull

import akka.actor.{ActorRef, ActorSystem}
import org.appdapter.fancy.log.VarargsLogging

/**
  * Created by Owner on 4/13/2016.
  */
object TestDullServer extends VarargsLogging {
	def main(args: Array[String]): Unit = {

		val standPumpTestCtxName = "standPumpCtx_333"
		val standPumpCtxActorRef : ActorRef = findStandAppPumpTopActor(standPumpTestCtxName)

		info1("Found standalone dull-pump topActor: {}", standPumpCtxActorRef)
	}
	private val akkaSysName = "DullStandSys_4719"
	lazy private val myAkkaSys = ActorSystem(akkaSysName)
	lazy private val myStandalonePumpSpace = new SpecialAppPumpSpace(myAkkaSys)
	def findStandAppPumpTopActor(topActrName : String) : ActorRef = {
		// Triggers creation and linking of both pumpCtx and topActor, as needed.
		myStandalonePumpSpace.findTopActorRef(topActrName)
	}
}

class SpecialAppPumpCtx extends SegregatedBoundedDullPumpCtx {

}
class SpecialAppPumpSpace(akkaSys : ActorSystem) extends SegregatedDullPumpSpace(akkaSys) {
	override protected def makeDullPumpCtx(topActorName : String) : DullPumpCtx = {
		new SpecialAppPumpCtx
	}
}

