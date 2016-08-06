/*
 *  Copyright 2016 by The Friendularity Project (www.friendularity.org).
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
package org.friendularity.field

import java.util.concurrent.TimeUnit
import akka.actor.{Scheduler => AkkaSched, Cancellable, ActorSystem, Props, ActorRef, ActorRefFactory, Actor}

import org.appdapter.fancy.log.VarargsLogging
import org.friendularity.cpmsg.{CPStrongTeller, CPumpMsg}

import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContextExecutor
import scala.concurrent.duration.{Duration, FiniteDuration}

/**
  * File created by Stub22 on 7/15/2016.
  */

case class SchedTaskRepeating(myTickMsg : CPumpMsg, initDelayDur : FiniteDuration,
							  periodDur : FiniteDuration) extends VarargsLogging {
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
	def makeSchedItemRepeating(msg : CPumpMsg, phaseMillis : Int, periodMillis : Int): SchedTaskRepeating = {
		val initDelayDur = Duration.create(phaseMillis, TimeUnit.MILLISECONDS)
		val periodDur = Duration.create(periodMillis, TimeUnit.MILLISECONDS)
		new SchedTaskRepeating(msg, initDelayDur, periodDur)
	}
}
trait StatusTickScheduler extends ScheduleHelper with VarargsLogging {
	def getTickPeriodMillis: Int = 600

	def makeReportingTickSchedTskRep(): SchedTaskRepeating = {
		val msg = new ReportingTickChance()
		val schedItemRep = makeSchedItemRepeating(msg, 0, getTickPeriodMillis)
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
		info1("Distributing status tick to {} targets", myCrudeTgts.length : Integer)
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
