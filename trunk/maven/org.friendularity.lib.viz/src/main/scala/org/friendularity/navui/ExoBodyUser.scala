package org.friendularity.navui

import java.util.concurrent.TimeUnit

import akka.actor.{Scheduler => AkkaSched, Cancellable, ActorSystem, Props, ActorRef, ActorRefFactory, Actor}
import akka.dispatch.{Dispatcher => AkkaDisp}
import org.friendularity.cpump.{CPStrongTeller, CPumpMsg}
import scala.concurrent.ExecutionContextExecutor
import akka.dispatch.Dispatcher
import org.appdapter.fancy.log.VarargsLogging
import org.friendularity.vwmsg.{VWBodyRq, VWExoBodyChance, VWBodyNotice}

import scala.concurrent.duration.{FiniteDuration, Duration}

/**
  * Created by Owner on 6/8/2016.
  */
// Even further out, beyond all the Outer stuff, we have Exo stuff, which is primarily client-standin test logic.

case class SchedItemRepeating(myTickMsg : CPumpMsg, initDelayDur : FiniteDuration, periodDur : FiniteDuration) extends VarargsLogging {
	def addToSched(sched: AkkaSched, cutionCtxCutor: ExecutionContextExecutor,
				   tgtActor: ActorRef, senderActor : ActorRef): Cancellable = {
		// Note second pair of args supplying  ExecutionContext and sender
		val cnclbl = sched.schedule(initDelayDur, periodDur, tgtActor, myTickMsg)(cutionCtxCutor, senderActor)
		info1("scheduling complere, handle={}", cnclbl)
		cnclbl
	}
	def addToSchedForSys(akkaSys: ActorSystem, tgtActor: ActorRef, senderActor : ActorRef) : Cancellable = {
		addToSched(akkaSys.scheduler, akkaSys.dispatcher, tgtActor, senderActor)
	}

}
trait ScheduleHelper extends VarargsLogging {
	def makeSchedItemRepeating(msg : CPumpMsg, phaseMillis : Int, periodMillis : Int): SchedItemRepeating = {
		val initDelayDur = Duration.create(phaseMillis, TimeUnit.MILLISECONDS)
		val periodDur = Duration.create(periodMillis, TimeUnit.MILLISECONDS)
		new SchedItemRepeating(msg, initDelayDur, periodDur)
	}

}

trait ExoBodyUserLogic extends ScheduleHelper with VarargsLogging {
	var myBodyTeller_opt : Option[CPStrongTeller[VWBodyRq]] = None
	def rcvBodyNotice(bodyNotice : VWBodyNotice): Unit = {
		info1("ExoBody UserLogic received bodyNotice={}", bodyNotice)
		val bodyTeller: CPStrongTeller[VWBodyRq] = bodyNotice.getBodyTeller
		myBodyTeller_opt = Option(bodyTeller)
	}

	def rcvUpdtick(exoBodyUpChance : VWExoBodyChance) : Unit = {
		info1("Exo body up chance received: {}", exoBodyUpChance)
		if (myBodyTeller_opt.isDefined) {
			// It is time to move my guy around, or whatever!
			rcvUpdtickForBody(exoBodyUpChance, myBodyTeller_opt.get)
		}
	}
	protected def rcvUpdtickForBody(exoBodyUpChance : VWExoBodyChance, bodyTeller : CPStrongTeller[VWBodyRq]) : Unit
	def makeRegularTickItem() : SchedItemRepeating = {
		val msg = new VWExoBodyChance{}
		makeSchedItemRepeating(msg, 0, getTickPeriodMillis)
	}
	def getTickPeriodMillis : Int = 1500
}

class ExoBodyUserActor(bodyUserLogic : ExoBodyUserLogic)  extends Actor {
	def receive = {
		case vwbn: VWBodyNotice => bodyUserLogic.rcvBodyNotice(vwbn)
		case vwebc : VWExoBodyChance => bodyUserLogic.rcvUpdtick(vwebc)
	}
}
object ExoActorFactory {

	def makeExoBodyUserActor(parentARF : ActorRefFactory, ebuActorName : String,
							 userLogic : ExoBodyUserLogic) : ActorRef = {
		val ebuActorProps = Props(classOf[ExoBodyUserActor], userLogic)
		val ebuActorRef : ActorRef = parentARF.actorOf(ebuActorProps, ebuActorName)

		ebuActorRef
	}

}