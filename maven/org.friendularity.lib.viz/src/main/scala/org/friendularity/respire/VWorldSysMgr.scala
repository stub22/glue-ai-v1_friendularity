package org.friendularity.respire

import akka.actor._
import org.appdapter.fancy.log.VarargsLogging
import org.friendularity.cpump.{ListenChanDirectActor, CPumpMsg, CPMsgTeller, CPAdminRequestMsg}
import org.friendularity.dull.DullPumpCtx

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


trait VWorldRequest extends CPumpMsg with VarargsLogging {
}

trait VWorldNotice extends CPumpMsg with VarargsLogging {

}
trait VWAdminRqMsg extends VWorldRequest {
	def processInSys(sysMgr : VWorldSysMgr, actCtx : ActorContext): Unit
}
case class VWARM_GreetFromPumpAdmin(pumpAdminTeller : CPMsgTeller) extends VWAdminRqMsg {
	override def processInSys(sysMgr : VWorldSysMgr, actCtx : ActorContext) : Unit = {
		info3("Processing {} message with sysMgr={} and actCtx={}", this, sysMgr, actCtx)
	}
}
case class VWARM_FindGoodyTeller(answerTeller: CPMsgTeller) extends VWAdminRqMsg {
	override def processInSys(sysMgr : VWorldSysMgr, actCtx : ActorContext) : Unit = {
		info3("Processing {} message with sysMgr={} and actCtx={}", this, sysMgr, actCtx)
	}
}
case class VWARM_FindPublicTellers(answerTeller: CPMsgTeller) extends VWAdminRqMsg {
	override def processInSys(sysMgr : VWorldSysMgr, actCtx : ActorContext) : Unit = {
		info3("Processing {} message with sysMgr={} and actCtx={}", this, sysMgr, actCtx)
		val pubTellers : VWorldPublicTellers = sysMgr.findPublicTellers
		answerTeller.tellCPMsg(pubTellers)
	}

}
trait VWContentRq extends VWorldRequest {
}
trait RdfMsg {
	def asTurtleString : String

	def asJenaModel(flags_opt: Option[AnyRef]) : AnyRef
	// def asR2goModel : AnyRef
}
trait VWGoodyRqRdf extends VWContentRq  with RdfMsg{

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
// Top level actor for handling only the grossest VWorld system start/restart/shutdown kinds of messages.
class VWorldBossActor[VWSM <: VWorldSysMgr](sysMgr : VWSM, hackStrap : VWorldStrap)
			extends Actor with ActorLogging with VWorldBossLogic[VWSM]{
	def receive = {
		// Construction of any other actors used with the ctx must happen within this handler.
		// Those actors may be sent back in receiptMsgs to answerTellers embedded in the input msg.
		// Note that "context" here is a pseudo-field of the actor.
		case vwmsg : VWorldRequest => processVWorldMsg(vwmsg, context)
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
