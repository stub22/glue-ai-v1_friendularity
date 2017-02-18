package org.friendularity.dull

import akka.actor._
import org.appdapter.core.name.{FreeIdent, Ident}
import org.appdapter.fancy.log.VarargsLogging
import org.friendularity.cpmsg.{CPMsgTeller, ActorRefCPMsgTeller, CreatedChanTellerMsg, CPumpMsg}
import org.friendularity.cpump._
import org.friendularity.vw.msg.adm.VWARM_GreetFromPumpAdmin

/**
  * Created by Stub22 on 4/13/2016.
  */
object TestDullServer extends VarargsLogging {
	val akkaSysName = "DullStandSys_4719"
	val standPumpTestCtxName = "standPumpCtx_333"

	def main(args: Array[String]): Unit = {

		val standPumpCtxActorRef : ActorRef = findStandAppPumpTopActor(standPumpTestCtxName)
		val standPumpAdminTeller = new ActorRefCPMsgTeller(standPumpCtxActorRef)

		info1("Found standalone dull-pump topActor: {}", standPumpCtxActorRef)
		val respConsumerActor : ActorRef = 	myAkkaSys.actorOf(Props[DullTestResponseConsumer], "ingrownDullRespCons")
		val answerTeller = new ActorRefCPMsgTeller[CreatedChanTellerMsg](respConsumerActor)


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

	}
	def simulateVWBossStartup(fromWhom : CPMsgTeller): Unit = {
		/*
		import org.friendularity.vw.impl.sys.VWorldActorFactoryFuncs

		val vwBossAR : ActorRef = VWorldActorFactoryFuncs.makeVWorldBoss(myAkkaSys, "vworldBoss_888")
		val vwBossTeller = new ActorRefCPMsgTeller(vwBossAR)
		val hpatMsg = new VWARM_GreetFromPumpAdmin(fromWhom)
		vwBossTeller.tellCPMsg(hpatMsg)
		info1("HelloPumpAdminTeller SENT to VWBossTeller : {}", vwBossTeller)
		*/
		// This answerTeller now requires strong type.
		//		val fptMsg = new VWARM_FindPublicTellers(answerTeller)
		//		vwBossTeller.tellCPMsg(fptMsg)
		//		info1("VWARM_FindPublicTellers SENT to VWBossTeller : {}", vwBossTeller)

	}

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


/*
My unconventional tip for today is that content designers and programmers all benefit from
continuing growth in thinking about data schemas, and how to combine and apply them cleanly.
Data  schemas  are used heavily and necessarily in both messaging and databases.   Absence of
schemas in these areas is a clear sign of incomplete work.    At a higher level, when we look
at any proposed feature,  we need to think about the data relationships it implies or requires,
and what other features might have to be added or changed to support that data.

[12:44]
As software projects mature, change, and are eventually retired or replaced, it is often the data
schemas that retain the most lasting value for the org.

stub22 [12:53 PM]
While at the beginning of the lifecycle, when defining a new product-oriented project,
3 main areas need the most clarity and explicit attention up front, in almost all modern systems:

a) User functional requirements
b) Authorability requirements, if any (can be thought of as meta-functional requirements)
c) Data requirements

[12:58]
As much as we know and can decide about all three of those is distilled into our data schemas,
which are the logical foundation of messaging and storage.  That is why I go on about data so
much.   Thank you America, and yes you are welcome, for me, that guy who talks about data.
 */