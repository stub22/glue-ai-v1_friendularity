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
case class HelloPumpAdminTeller(pumpAdminTeller : CPMsgTeller) extends VWAdminRqMsg {
	override def processInSys(sysMgr : VWorldSysMgr, actCtx : ActorContext) : Unit = {
		info3("Processing {} message with sysMgr={} and actCtx={}", this, sysMgr, actCtx)
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
trait VWorldPublicSummary extends VWorldNotice {
	def getFirstBigTeller : CPMsgTeller
	def getSecondCoolTeller : CPMsgTeller

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
		case vwmsg : VWorldRequest =>
	}

	override protected def getSysMgr : VWSM = sysMgr
}
class VWSysMgrImpl extends VWorldSysMgr {

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
