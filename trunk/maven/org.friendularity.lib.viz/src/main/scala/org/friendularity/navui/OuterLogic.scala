package org.friendularity.navui

import java.util.Random
import java.lang.{Long => JLong}
import akka.actor.{ActorRefFactory, ActorContext, ActorRef}
import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.api.fancy.FancyThingModelWriter
import org.cogchar.render.rendtest.GoodyTestMsgMaker
import org.friendularity.cpump.{ActorRefCPMsgTeller, CPStrongTeller, CPMsgTeller}
import org.friendularity.respire.{VWGoodyRqTurtle, VWGoodyRqTAS, MsgJobLogic, MsgJobLogicFactory, VWorldMasterFactory, VWCharAdminRq, VWorldPublicTellers}

import com.hp.hpl.jena.rdf.model.{Model => JenaModel, ModelFactory => JenaModelFactory, Literal}


/**
  * Created by Owner on 6/8/2016.
  */
trait OuterLogic extends VarargsLogging {
	def rcvPubTellers (vwpt : VWorldPublicTellers): Unit
}
trait PatientSender_GoodyTest extends OuterLogic {
	import scala.collection.JavaConverters._

	lazy val myRandomizer: Random = new Random

	def finallySendGoodyTstMsgs(goodyTeller : CPMsgTeller, flag_serToTurtle : Boolean): Unit = {

		val gtmm: GoodyTestMsgMaker = new GoodyTestMsgMaker
		val msgsJList = gtmm.makeGoodyCreationMsgs
		val msgsScbuf = msgsJList.asScala

		for (actSpec <- msgsScbuf) {
			if (flag_serToTurtle) {
				val ftmw = new FancyThingModelWriter
				val specModelWithPrefixes : JenaModel  = ftmw.writeTASpecAndPrefixesToNewModel(actSpec, myRandomizer)

				val turtleTriplesString : String = ftmw.serializeSpecModelToTurtleString(specModelWithPrefixes)
				info2("Serialized turtle message FROM model of size {} triples TO string of length {} chars", specModelWithPrefixes.size() : JLong, turtleTriplesString.length : Integer)
				debug1("Dumping serialized turtle message before send:\n {}", turtleTriplesString)
				val turtleMsg = new VWGoodyRqTurtle(turtleTriplesString)
				goodyTeller.tellCPMsg(turtleMsg)
			} else {
				getLogger.info("Wrapping java-serializable message: {}", actSpec)
				val vwMsgWrap = new VWGoodyRqTAS(actSpec)
				goodyTeller.tellCPMsg(vwMsgWrap)
			}
		}
	}
	private var myStoredTellers_opt : Option[VWorldPublicTellers] = None

	private val useTurtleSerialization : Boolean = true
	override def rcvPubTellers (vwpt : VWorldPublicTellers): Unit = {
		// This is the (outbound) notice we get back from boss, confirming system has started and these tellers are ready for biz.
		info1("Outer logic got public tellers: {}", vwpt)
		myStoredTellers_opt = Option(vwpt)
		val goodyTeller = vwpt.getGoodyTeller
		if (goodyTeller.isDefined) {
			info1("Sending goody tst msgs to: {}", goodyTeller.get)
			finallySendGoodyTstMsgs(goodyTeller.get, useTurtleSerialization)
		} else {
			warn0("GoodyTeller is not available, cannot send goody tst msgs.")
		}

	}
}
trait PatientForwarder_CharAdminTest extends OuterLogic {
	private var myStoredTellers_opt : Option[VWorldPublicTellers] = None
	private val myPendingCharAdminRqs = new scala.collection.mutable.ListBuffer[VWCharAdminRq]() //  = Nil // Any inbound messages we have not gotten to yet.

	override def rcvPubTellers(vwpt: VWorldPublicTellers): Unit = {
		myStoredTellers_opt = Option(vwpt)
		propagateMessages(myStoredTellers_opt)
	}

	def appendInboundRq(rqMsg : VWCharAdminRq) : Unit = {
		synchronized {
			myPendingCharAdminRqs.append(rqMsg)
			info1("After append, pending charAdm msgs={}", myPendingCharAdminRqs)
			if (myStoredTellers_opt.isDefined) {
				propagateMessages(myStoredTellers_opt)
			}
		}
	}
	def propagateMessages(pubTellers_opt : Option[VWorldPublicTellers]) : Unit = {
		this.synchronized {
			if (pubTellers_opt.isDefined) {
				val charAdminTeller_opt = pubTellers_opt.get.getCharAdminTeller
				if (charAdminTeller_opt.isDefined) {
					val charAdminTeller = charAdminTeller_opt.get
					while (myPendingCharAdminRqs.nonEmpty) {
						val headRQ = myPendingCharAdminRqs.head
						info1("Forwarding pending charAdmRq={}", headRQ)
						charAdminTeller.tellCPMsg(headRQ)
						myPendingCharAdminRqs.remove(0) //  = myPendingCharAdminRqs.tail
					}

				}
			}
		}
	}
}


// Unnecessary to use the Jobby approach here, but working through it anyway as an excercise.
class OuterJobbyWrapper(outerLogic : OuterLogic) extends MsgJobLogic[VWorldPublicTellers] {
	// Differences here is that we get exception handling+logging, runtime type verification,
	// and actor wrapping for free, but we must also create the factory stuff below.
	// Note that we could also pass constructor parameters in via the factory, without Props hassles.
	override def processMsgUnsafe(msg : VWorldPublicTellers, slf : ActorRef, sndr : ActorRef,
								  actx : ActorContext) : Unit = {
		debug2("Received public-tellers-ready msg={} for outerLogic={}", msg, outerLogic)
		msg match {
			case vwpt : VWorldPublicTellers => 	outerLogic.rcvPubTellers(vwpt)
		}

	}
}
// Now we would make the factories needed to construct our logic + actor instances.
// Question is:  When is this less bother than making an actor wrapper by hand, as in OuterDirectActor above?

object OuterJobbyLogic_MasterFactory extends VWorldMasterFactory {
	// val thatOtherParam : Int = 22
	val oolFactory = new MsgJobLogicFactory[VWorldPublicTellers, OuterLogic]() {
		override def makeJobLogic(olJobArg : OuterLogic, msgFilterClz: Class[VWorldPublicTellers]): MsgJobLogic[VWorldPublicTellers] = {
			info2("Making jobby wrapper for outerlogic-jobArg={} for specific runtime filter clz: {}", olJobArg,  msgFilterClz)
			new OuterJobbyWrapper(olJobArg)
		}
	}
	val oolFactPair = makeFactoryPair[VWorldPublicTellers, OuterLogic](classOf[VWorldPublicTellers], oolFactory)

	val oolJobbyActorName = "outer_jobby"

	def makeOoLogicAndTeller(jobArg : OuterLogic,  arf : ActorRefFactory, actorName : String) : CPStrongTeller[VWorldPublicTellers] = {
		val aref = oolFactPair.makeLogicAndActor(jobArg, arf, actorName, None)
		new ActorRefCPMsgTeller[VWorldPublicTellers](aref)
	}
}
