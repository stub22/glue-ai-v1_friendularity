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
case class CachedActorRefTeller[TargetKind, MsgKind <: AnyRef ](myActorRef : ActorRef) extends Teller[TargetKind, MsgKind] {
	val myRefOpt = Option(myActorRef)
	override def getKnownActRef_opt = myRefOpt
}
case class SelectionTeller(myActorSelection : ActorSelection) {

}

trait TellerFactory[TargetKind, MsgKind <: AnyRef] {
	def makeTellerAndActor(actrCtx : ActorContext, props : Props, rcvr : Receiver[MsgKind], reportTo: Teller[_, AdminMsg]): CachedActorRefTeller[TargetKind, MsgKind] = {

		null
	}
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

