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
package org.friendularity.navui

import java.util.concurrent.TimeUnit

import akka.actor.{Props, ActorRef, ActorRefFactory, Actor}
// import akka.dispatch.{Dispatcher => AkkaDisp}
import org.friendularity.cpump.{CPStrongTeller, CPumpMsg}
import org.friendularity.field.{ScheduleHelper, SchedTaskRepeating}
// import scala.concurrent.ExecutionContextExecutor
// import akka.dispatch.Dispatcher
import org.appdapter.fancy.log.VarargsLogging
import org.friendularity.vwmsg.{VWBodyRq, VWExoBodyChance, VWBodyNotice}

// import scala.concurrent.duration.{FiniteDuration, Duration}

/**
  * Created by Stub22 on 6/8/2016.
  */
// Even further out, beyond all the Outer stuff, we have Exo stuff, which is primarily client-standin test logic.


// We expect to have one such UserLogic for *each* separate VWBody created.
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

	// Called from   NavUiAppSvc.makeExoBodyUserTeller_withTicks
	def makeRegularTickItem() : SchedTaskRepeating = {
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