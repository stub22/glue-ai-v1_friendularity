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

package org.friendularity.respire

import akka.actor._
import org.appdapter.fancy.log.VarargsLogging
import org.friendularity.cpump.{ListenChanDirectActor, CPumpMsg, CPMsgTeller, CPAdminRequestMsg}


/**
  * Created by Owner on 4/14/2016.
  */

// Legit state of a running VWorld system is managed by an instance of VWorldSysMgr.
trait VWorldSysMgr {
	def findPublicTellers : VWorldPublicTellers


}
// (hack)Strap holds any icky extra shared state that we temporarily want to pass to VWorldBossActor.
trait VWorldStrap {
//	def	getPumpCtx : DullPumpCtx
}

// The vworldBoss supplies this serializable directory of its actors to any client who asks.
// From here clients can navigate to all published vworld service actors.
// Client may also know+find these same actors and other actors by other means.
// This trait is made available as a helpful starting point, not as the definitive or exhaustive API.
trait VWorldPublicTellers extends VWorldNotice {
	def getFirstBigTeller : Option[CPMsgTeller] = None
	def getSecondCoolTeller : Option[CPMsgTeller] = None
	def getGoodyTeller : Option[CPMsgTeller] = None
}
trait VWorldBossLogic [VWSM <: VWorldSysMgr] {
	protected def getSysMgr : VWSM

	protected def processVWorldMsg (vwmsg : VWorldRequest, localActorCtx : ActorContext): Unit = {
		val sysMgr = getSysMgr
		vwmsg match {
			case adminMsg : VWAdminRqMsg => {
				adminMsg.processInSys(getSysMgr, localActorCtx)
			}
			case goodyMsg : VWGoodyRqRdf => processVWGoodyRdfMsg(goodyMsg, localActorCtx)
		}
	}
	protected def processVWGoodyRdfMsg (goodyMsg : VWGoodyRqRdf, localActorCtx : ActorContext) : Unit = {

	}
}
// Top level actor, directly handles only the grossest VWorld system start/restart/shutdown
// kinds of messages.  More importantly, boss serves as top actor supervisor for all internal
// vworld actors.   Conceptually it also marks the inside and the outside of the vworld.
// The sysMgr and hackStrap are the *only* places that any complex state higgledy-piggledy
// may take place, and it is our goal to minimize that state and code.  (That's why we give
// these things names.  Shared state is bad, so we won't let it be vague or implicit!).

// Child actors of this boss should not accept unserializable constructor args (or messages).

class VWorldBossActor[VWSM <: VWorldSysMgr](sysMgr : VWSM, hackStrap : VWorldStrap)
			extends Actor with ActorLogging with VWorldBossLogic[VWSM]  {
	def receive = {
		// Construction of any other actors used with the ctx must happen within this handler.
		// Those actors may be sent back in receiptMsgs to answerTellers embedded in the input msg.
		// Note that "context" here is a pseudo-field of the actor.
		case vwmsg : VWorldRequest => {
			processVWorldMsg(vwmsg, context)
		}
	}

	override protected def getSysMgr : VWSM = sysMgr

}
class VWSysMgrImpl extends VWorldSysMgr {
	override def findPublicTellers : VWorldPublicTellers = new VWorldPublicTellers{}
}
class VWStrapImpl extends VWorldStrap {

}
object VWorldBossFactory {
	def makeVWorldBoss(akkaSys: ActorSystem, bossActorName : String) : ActorRef = {
		val vwstrap = new VWStrapImpl
		val vwsys = new VWSysMgrImpl
		val vwbossActorProps = Props(classOf[VWorldBossActor[VWorldSysMgr]], vwsys, vwstrap)
		val vwbActorRef : ActorRef = akkaSys.actorOf(vwbossActorProps, bossActorName)
		vwbActorRef
	}
}

