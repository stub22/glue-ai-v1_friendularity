package org.friendularity.infra.field

import akka.actor.{Props, ActorRefFactory, Actor, ActorRef, ActorSystem}
import org.appdapter.fancy.log.VarargsLogging
import org.friendularity.infra.cpmsg.CPStrongTeller
import org.friendularity.infra.cpump.{SchedTaskRepeating, ScheduleHelper}

import scala.collection.mutable.ListBuffer

/**
  * Created by Owner on 8/9/2016.
  */
trait StatusTickScheduler extends ScheduleHelper with VarargsLogging {
	def getTickInitDelayMillis = 3000
	def getTickPeriodMillis: Int = 750

	def makeReportingTickSchedTskRep(): SchedTaskRepeating = {
		val msg = new ReportingTickChance()
		val schedItemRep = makeSchedItemRepeating(msg, getTickInitDelayMillis, getTickPeriodMillis)
		schedItemRep
	}

	val myRunningTasks = new ListBuffer[SchedTaskRepeating] // For debugging and/or later cancelling

	def scheduleReportingTicks(akkaSys: ActorSystem, tgtActor: ActorRef, senderActor: ActorRef): Unit = {
		val repTskRep = makeReportingTickSchedTskRep()
		repTskRep.addToSchedForSys(akkaSys, tgtActor, senderActor)
		myRunningTasks.append(repTskRep)
	}
}
trait StatusTickDistributor extends VarargsLogging {
	val myCrudeTgts = new ListBuffer[CPStrongTeller[ReportingTickChance]]
	def registerTickLover(srcTeller : CPStrongTeller[ReportingTickChance]): Unit = {
		myCrudeTgts.append(srcTeller)
	}
	def handleReceivedTick(rcvdTick : ReportingTickChance) : Unit = {
		debug1("Distributing status tick to {} targets", myCrudeTgts.length : Integer)
		for (tickLover <- myCrudeTgts) {
			tickLover.tellStrongCPMsg(rcvdTick)
		}
	}
}
class StatusTickDistribActor(distrib : StatusTickDistributor) extends Actor {
	override def receive : Actor.Receive = {
		case rtc : ReportingTickChance => {
			distrib.handleReceivedTick(rtc)
		}
	}
}
object StatusTickActorFactory {
	def makeStatusTickDistribActor(parentARF : ActorRefFactory, tdActorName : String,
								   distrib : StatusTickDistributor) : ActorRef = {
		val distribActorProps = Props(classOf[StatusTickDistribActor], distrib)
		val distribActorRef : ActorRef = parentARF.actorOf(distribActorProps, tdActorName)
		distribActorRef
	}

}
