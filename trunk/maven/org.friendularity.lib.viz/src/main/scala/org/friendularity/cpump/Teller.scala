package org.friendularity.cpump


import java.io.{Serializable => JSerializable}

import akka.actor.{Props, ActorContext, ActorSelection, ActorRef}
import org.appdapter.fancy.log.VarargsLogging

/**
  * Created by Owner on 4/10/2016.
  */
trait Teller[TargetKind, MsgKind <: AnyRef] extends JSerializable {
	protected def getKnownActRef_opt : Option[ActorRef]
	// If invoked in actor context, sender will be implicitly hooked up.
	def tellNowFromImplicitSender(msg: MsgKind) : Boolean = {
		val actRefOpt = getKnownActRef_opt
		if (actRefOpt.isDefined) {
			actRefOpt.get ! msg
			true
		} else false
	}
	def checkAndReport(callback : Function0[Boolean]) = ???
}

trait Receiver[MsgKind <: AnyRef] extends JSerializable {
	def doRecieve(msg: MsgKind)
}

class FirstReceiver[MsgKind <: AnyRef] extends Receiver[MsgKind] with VarargsLogging {
	override def doRecieve(msg: MsgKind) {
		info2("Receiver={} got msg={}", this, msg : AnyRef)
	}
}

trait WackyTeller[MsgKind <: AnyRef] extends Teller[Receiver[MsgKind], MsgKind]


class ZoomTeller[MsgKind <: AnyRef] {

}

