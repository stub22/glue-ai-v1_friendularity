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
import org.friendularity.cpump.CPumpMsg

import scala.concurrent.ExecutionContextExecutor
import scala.concurrent.duration.{Duration, FiniteDuration}

/**
  * File created by Stub22 on 7/15/2016.
  */

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
