package org.friendularity.dull

import akka.actor._
import org.appdapter.core.name.{FreeIdent, Ident}
import org.appdapter.fancy.log.VarargsLogging
import org.friendularity.cpump._
import org.friendularity.respire.{VWARM_FindPublicTellers, VWARM_GreetFromPumpAdmin, VWorldBossFactory}

/**
  * Created by Owner on 4/13/2016.
  */
object TestDullServer extends VarargsLogging {
	def main(args: Array[String]): Unit = {

		val standPumpTestCtxName = "standPumpCtx_333"
		val standPumpCtxActorRef : ActorRef = findStandAppPumpTopActor(standPumpTestCtxName)
		val standPumpAdminTeller = new ActorRefCPMsgTeller(standPumpCtxActorRef)

		info1("Found standalone dull-pump topActor: {}", standPumpCtxActorRef)
		val respConsumerActor : ActorRef = 	myAkkaSys.actorOf(Props[DullTestResponseConsumer], "ingrownDullRespCons")
		val answerTeller = new ActorRefCPMsgTeller(respConsumerActor)


		val listenChanID : Ident = new FreeIdent("http://onto.friendularity.org/testchans#listenChanDD");
		val listenedMsgClz = classOf[CPumpMsg]
		val adoptrs = Nil
		val rqMakeListChan = new CPARM_MakeDullListenChan[CPumpMsg](listenChanID, listenedMsgClz, adoptrs, answerTeller)

		standPumpAdminTeller.tellCPMsg(rqMakeListChan)

		info1("Request SENT to make listenChan for ID={}", listenChanID)
//			adoptrs : Traversable[CPumpAdptr[LMK, DullPumpCtx, CPumpMsg]],
//			receiptTeller: CPReceiptTeller)
		lazy val dispPostChanID : Ident = new FreeIdent("http://onto.friendularity.org/testchans#postChan017");
		val postedMsgClz = classOf[CPumpMsg]
		val rqMakePostChan = new CPARM_MakeDullPostDispatchChan[CPumpMsg](dispPostChanID, postedMsgClz, answerTeller)
		standPumpAdminTeller.tellCPMsg(rqMakePostChan)
		info1("Request SENT to make dispatch-post chan for ID={}", dispPostChanID)

		val vwBossAR : ActorRef = VWorldBossFactory.makeVWorldBoss(myAkkaSys, "vworldBoss_888")
		val vwBossTeller = new ActorRefCPMsgTeller(vwBossAR)
		val hpatMsg = new VWARM_GreetFromPumpAdmin(standPumpAdminTeller)
		vwBossTeller.tellCPMsg(hpatMsg)
		info1("HelloPumpAdminTeller SENT to VWBossTeller : {}", vwBossTeller)
		val fptMsg = new VWARM_FindPublicTellers(answerTeller)
		vwBossTeller.tellCPMsg(fptMsg)
		info1("VWARM_FindPublicTellers SENT to VWBossTeller : {}", vwBossTeller)
	}
	private val akkaSysName = "DullStandSys_4719"
	lazy private val myAkkaSys = ActorSystem(akkaSysName)
	lazy private val myStandalonePumpSpace = new SpecialAppPumpSpace(myAkkaSys)
	def findStandAppPumpTopActor(topActrName : String) : ActorRef = {
		// Triggers creation and linking of both pumpCtx and topActor, as needed.
		myStandalonePumpSpace.findTopActorRef(topActrName)
	}
}

class SpecialAppPumpCtx extends SegregatedBoundedDullPumpCtx {

}
class SpecialAppPumpSpace(akkaSys : ActorSystem) extends SegregatedDullPumpSpace(akkaSys) {
	override protected def makeDullPumpCtx(topActorName : String) : DullPumpCtx = {
		new SpecialAppPumpCtx
	}
}

class DullTestResponseConsumer  extends Actor with ActorLogging {
	def receive = {
		case cpmsg : CPumpMsg => {
			log.info("Received response CPumpMsg : {}", cpmsg)
		}
		case omsg => {
			log.warning("Received non-cpump msg: {}", omsg)
		}
	}
}


