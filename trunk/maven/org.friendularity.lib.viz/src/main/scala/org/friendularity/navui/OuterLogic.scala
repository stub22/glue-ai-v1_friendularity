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

import java.util.{Random => JRandom}
import java.lang.{Long => JLong}
import akka.actor.{ActorSystem, ActorRefFactory, ActorContext, ActorRef}
import com.jme3.math.{Quaternion, Vector3f, ColorRGBA}
import org.appdapter.core.name.Ident
import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.api.fancy.FancyThingModelWriter
import org.cogchar.impl.thing.basic.BasicThingActionSpec
import org.cogchar.render.rendtest.GoodyTestMsgMaker
import org.friendularity.cpmsg.{ActorRefCPMsgTeller, CPStrongTeller, CPMsgTeller}

import org.friendularity.field.{ReportingPolicy, ReportSrcOpen, SourceDataMsg, MsgToStatusSrc, StatusTickActorFactory, ReportingTickChance, StatusTickDistributor, StatusTickScheduler, FieldActorFactory}
import org.friendularity.mjob.{MsgJobLogicFactory, MsgJobLogic}

import com.hp.hpl.jena.rdf.model.{Model => JenaModel, ModelFactory => JenaModelFactory, Literal}
import org.friendularity.vwimpl.{OverlayPage, IdentHlp, VWorldMasterFactory}

import scala.collection.immutable.HashMap

import org.friendularity.vwmsg.{VWOverlayRq, VWSetupOvlBookRq, NavCmdImpl, NavCmdKeyClkBind, NavCmd, InnerNavCmds, VWorldPublicTellers, VWSCR_Node, VWBindCamNodeRq, VWCreateCamAndViewportRq, CamStateParams3D, CamState3D, ViewportDesc, ShapeManipRqImpl, SmooveManipEndingImpl, TransformParams3D, VWBodySkeletonDisplayToggle, VWBroadcastToAllBodies, VWClearAllShapes, VWStageResetToDefault, VWKeymapBinding_Medial, OrdinaryParams3D, VWSCR_Sphere, VWStageOpticsBasic, VWSCR_CellGrid, VWStageEmulateBonusContentAndCams, VWBodyLifeRq, VWGoodyRqTAS, VWGoodyRqTurtle}


/**
  * Created by Stu B22 - June 2016
  */
trait OuterLogic extends VarargsLogging {
	// Tells the OuterLogic that the main public services (setup by VWorld boss) are now ready.
	def rcvPubTellers (vwpt : VWorldPublicTellers): Unit

	// Normally an OuterLogic will call doAllExtraSetup at the end of its rcvPubTellers impl.
	// Adding setup tasks onto
	protected def doAllExtraSetup(vwpt : VWorldPublicTellers) : Unit = {
		val extraSetupTasks = getExtraSetupTasks
		extraSetupTasks.map(
			_.doExtraSetup(vwpt)
		)
	}

	// Subclasses can effectively add tasks to be executed by overriding getExtraSetupTasks.
	protected def getExtraSetupTasks() : List[ExtraSetupLogic] = Nil
}
trait TurtleSerHlp extends VarargsLogging {
	def serlzOneTAMsgToTurtleTxt(actSpec : BasicThingActionSpec, rand : JRandom) : String = {
		val ftmw = new FancyThingModelWriter
		val specModelWithPrefixes : JenaModel  = ftmw.writeTASpecAndPrefixesToNewModel(actSpec, rand)

		val triplesTurtleTxt : String = ftmw.serializeSpecModelToTurtleString(specModelWithPrefixes)
		info2("Serialized turtle message FROM model of size {} triples TO string of length {} chars",
			specModelWithPrefixes.size() : JLong, triplesTurtleTxt.length : Integer)
		debug1("Dumping serialized turtle message before send:\n {}", triplesTurtleTxt)
		triplesTurtleTxt
	}

}
trait PatientSender_GoodyTest extends OuterLogic with IdentHlp with TurtleSerHlp {
	import scala.collection.JavaConverters._

	lazy val myRandomizer: JRandom = new JRandom

	def sendBigGridRq(shapeTeller : CPMsgTeller): Unit = {
		val rq_makeBigGrid = new VWSCR_CellGrid{}
		shapeTeller.tellCPMsg(rq_makeBigGrid)
	}

	def sendOvalRq_MAKE(shapeTeller : CPMsgTeller) : Ident = {
		val sphereCol = new ColorRGBA(0.1f,1.0f,0.5f, 0.65f) // aqua
		val spherePos = new Vector3f(-15.0f, 12.0f, 4.0f) // biggish dirigible
		val sphereRot = Quaternion.IDENTITY
		val sphereParams = new OrdinaryParams3D(spherePos, sphereRot, Vector3f.UNIT_XYZ, sphereCol)
		val knownSphereID_opt : Option[Ident] = Some(makeStampyRandyIdent())
		val rq_makeSphere = new VWSCR_Sphere(9.0f, sphereParams, knownSphereID_opt)
		shapeTeller.tellCPMsg(rq_makeSphere)
		knownSphereID_opt.get
	}
	def makeArbXform() : TransformParams3D = {
		val tgtPos = new Vector3f(-25.0f, 45.0f, 6.0f)
		val tgtRot = Quaternion.IDENTITY
		val tgtScale = new Vector3f(1.0f, 0.5f, 4.0f)
		val tgtXform = new TransformParams3D(tgtPos, tgtRot, tgtScale)
		tgtXform
	}
	def sendShpSmooveRq(shapeTeller : CPMsgTeller, shapeID : Ident,
						tgtXform : TransformParams3D, durSec : Float) : Unit = {
		val endingManip = new SmooveManipEndingImpl(tgtXform, durSec)
		val sphereManipMsg = new ShapeManipRqImpl(shapeID, endingManip)
		shapeTeller.tellCPMsg(sphereManipMsg)
	}
	def finallySendFunShapeRqs(shapeTeller : CPMsgTeller) : Unit = {
		sendBigGridRq(shapeTeller)
		val bigOvalID = sendOvalRq_MAKE(shapeTeller)
		val tgtXform = makeArbXform()
		sendShpSmooveRq(shapeTeller, bigOvalID, tgtXform, 40.0f)
	}

	def finallySendGoodyTstMsgs(goodyTeller : CPMsgTeller, flag_serToTurtle : Boolean): Unit = {
		val gtmm: GoodyTestMsgMaker = new GoodyTestMsgMaker
		val msgsJList = gtmm.makeGoodyCreationMsgs

		val msgsScbuf : List[BasicThingActionSpec] = msgsJList.asScala.toList // BTAS is known to be serializable

		for (actSpec <- msgsScbuf) {
			if (flag_serToTurtle) {
//				val ftmw = new FancyThingModelWriter
//				val specModelWithPrefixes : JenaModel  = ftmw.writeTASpecAndPrefixesToNewModel(actSpec, myRandomizer)
//				val turtleTriplesString : String = ftmw.serializeSpecModelToTurtleString(specModelWithPrefixes)
				val turtleTxt = serlzOneTAMsgToTurtleTxt(actSpec, myRandomizer)
				val turtleMsg = new VWGoodyRqTurtle(turtleTxt)
				goodyTeller.tellCPMsg(turtleMsg)
			} else {
				getLogger.info("Sending java-serializable TA message: {}", actSpec)
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
		val goodyTeller = vwpt.getGoodyDirectTeller
		if (goodyTeller.isDefined) {
			info1("Sending goody tst msgs to: {}", goodyTeller.get)
			finallySendGoodyTstMsgs(goodyTeller.get, useTurtleSerialization)
		} else {
			warn0("GoodyTeller is not available, cannot send goody tst msgs.")
		}
		val shapeTeller = vwpt.getShaperTeller
		if (shapeTeller.isDefined) {
			finallySendFunShapeRqs(shapeTeller.get)
		}
		doAllExtraSetup(vwpt)
	}
}

trait PatientForwarder_CharAdminTest extends OuterLogic {
	private var myStoredTellers_opt : Option[VWorldPublicTellers] = None
	private val myPendingCharAdminRqs = new scala.collection.mutable.ListBuffer[VWBodyLifeRq]() //  = Nil // Any inbound messages we have not gotten to yet.

	override def rcvPubTellers(vwpt: VWorldPublicTellers): Unit = {
		myStoredTellers_opt = Option(vwpt)
		propagateMessages(myStoredTellers_opt)
		doAllExtraSetup(vwpt)
	}

	def appendInboundRq(rqMsg : VWBodyLifeRq) : Unit = {
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
			// Top part of this method could easily be shortened to a few lines of functional piping, sure.
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
trait PatientSender_BonusStaging extends OuterLogic with IdentHlp {
	override def rcvPubTellers(vwpt: VWorldPublicTellers): Unit = {
		val stageTeller = vwpt.getStageTeller.get

		val moveSpeed : Int = 25
		val bgColor = ColorRGBA.Yellow
		val opticsBasicRq = new VWStageOpticsBasic(moveSpeed, bgColor)
		stageTeller.tellCPMsg(opticsBasicRq)

		val emuBonusRq = new VWStageEmulateBonusContentAndCams()
		stageTeller.tellStrongCPMsg(emuBonusRq)

		setupStatusPumps(vwpt)

		setupOverlayBook(vwpt)

		setupKeysAndClicks(vwpt)

		sendExtraCameraRqs(stageTeller, vwpt.getShaperTeller.get)

		doAllExtraSetup(vwpt)
	}
	//
	def sendStageReset(stageTeller : CPMsgTeller) : Unit = {
		val stgResetMsg = new VWStageResetToDefault()
		stageTeller.tellCPMsg(stgResetMsg)
	}
	def sendClearKeysAndClicks(stageTeller : CPMsgTeller) : Unit = ???
	def sendClearShaps(shapeTeller : CPMsgTeller) : Unit = {
		val clrShpsMsg = new VWClearAllShapes
		shapeTeller.tellCPMsg(clrShpsMsg)
	}

	def sendToggleSkelHilite(cadmTeller : CPMsgTeller) : Unit = {
		val innerBodyRq = new VWBodySkeletonDisplayToggle
		val brdcstRq = new VWBroadcastToAllBodies(innerBodyRq)
		cadmTeller.tellCPMsg(brdcstRq)
	}
	// Cmds sent to navTeller

	def setupOverlayBook(vwpt: VWorldPublicTellers) : Unit = {

		val pages : List[OverlayPage] = NavPageDefs.pageList
		val ovlTeller = vwpt.getOverlayTeller.get

		val ovlSetupMsg = new VWSetupOvlBookRq(pages)

		ovlTeller.tellStrongCPMsg(ovlSetupMsg)
	}

	def setupStatusPumps(vwpt: VWorldPublicTellers) : Unit = {}

	def setupKeysAndClicks(vwpt: VWorldPublicTellers) : Unit = {

		val stageTeller = vwpt.getStageTeller.get

		// sendClearKeysAndClicks(stageTeller)

		// Define callback functions to be invoked in response to keyboard down stroke events.
		val funcStgRst = (pt : VWorldPublicTellers) => {sendStageReset(pt.getStageTeller.get)}

		val funcClrShps = (pt : VWorldPublicTellers) => {sendClearShaps(pt.getShaperTeller.get)}

		val inlineMap : Map[String,Function1[VWorldPublicTellers,Unit]] = Map("K" -> funcStgRst, "L" -> funcClrShps)
		// val typedMap =  HashMap(inlineMap)
		info1("inlineKeymap={}", inlineMap) // , typedMap)
		val regMsg = new VWKeymapBinding_Medial(inlineMap, vwpt)
		stageTeller.tellCPMsg(regMsg)

		val funcSkelHiliteToggle = (pt : VWorldPublicTellers) => {sendToggleSkelHilite(pt.getCharAdminTeller.get)}

		val nextMap = Map("F2" -> funcSkelHiliteToggle)
		val regMsg2 = new VWKeymapBinding_Medial(nextMap, vwpt)
		stageTeller.tellCPMsg(regMsg2)

		val outerKeysWidg = new OuterBindNavCmdKeys{}
		val kcmdReg = new VWKeymapBinding_Medial(outerKeysWidg.navCmdKeyBindMap, vwpt)
		stageTeller.tellCPMsg(kcmdReg)
	}
	def sendExtraCameraRqs(stageTeller : CPMsgTeller, spcTeller : CPMsgTeller): Unit = {
		val vpd = new ViewportDesc(0.2f, 0.4f, 0.15f, 0.30f, Some(ColorRGBA.Pink))
		val cpv = new Vector3f(-70.0f, 5.0f, -3.0f)
		val pdir = new Vector3f(1.0f, 0.0f, 0.0f)
		val cst = new CamStateParams3D(cpv, pdir)
		val camID : Ident = makeStampyRandyIdent
		val makeCamRq = new VWCreateCamAndViewportRq(camID, cst, vpd)
		stageTeller.tellCPMsg(makeCamRq)

		val camGuideNodeID : Ident = makeStampyRandyIdent
		info1("Requesting cam-guide node at ID={}", camGuideNodeID)
		val makeGuideNodeRQ = new VWSCR_Node(camGuideNodeID, None)
		spcTeller.tellCPMsg(makeGuideNodeRQ)

		val guideIsParent = true
		val camGuideBindRq = new VWBindCamNodeRq(camID, guideIsParent, spcTeller, camGuideNodeID)
		stageTeller.tellCPMsg(camGuideBindRq)

		val guideTgtPos = new Vector3f(-1.0f, 5.0f, 3.0f)
		val rotAngles = Array(45.0f, -45.0f, 15.0f)
		val guideTgtRot = new Quaternion(rotAngles)
		val guideTgtScale = Vector3f.UNIT_XYZ
		val guideTgtXform = new TransformParams3D(guideTgtPos, guideTgtRot, guideTgtScale)
		val endingManip = new SmooveManipEndingImpl(guideTgtXform, 60.0f)
		val guideManipMsg = new ShapeManipRqImpl(camGuideNodeID, endingManip)
		spcTeller.tellCPMsg(guideManipMsg)

	}
}
// Serves as a rendezvous
trait OuterAppPumpSetupLogic extends OuterLogic with IdentHlp with StatusTickScheduler {
	protected def getAkkaSystem : ActorSystem

	// Bypasses the whole Distrib thing, encoding its own gathering+publishing chain out to AMQP topic
	protected def makePubStatTempBypassTeller_opt(vwpt: VWorldPublicTellers) : Option[CPStrongTeller[MsgToStatusSrc]] = None

	def setupDownstreamActors_UnusedRightNow(ovlTeller : CPStrongTeller[VWOverlayRq]) : CPMsgTeller = {
		warn1("No setup is done yet to actually route mesages to ovlTeller: {}", ovlTeller)
		val downstreamTeller = FieldActorFactory.makeDistribIndep(getAkkaSystem, "downstreamFieldDistrib")
		downstreamTeller
	}
	def setupOuterPumpActors(tickLovinTellers : List[CPStrongTeller[ReportingTickChance]]) : Unit = {
		val statusTickHandler = new StatusTickDistributor{}
		tickLovinTellers.map(statusTickHandler.registerTickLover(_))
		val akkaSys = getAkkaSystem
		val statusTickActorRef = StatusTickActorFactory.makeStatusTickDistribActor(akkaSys, "statusTickHandler", statusTickHandler)
		info1("Made statusTickHandler-Actor: {}", statusTickActorRef)
		scheduleReportingTicks(akkaSys, statusTickActorRef, statusTickActorRef)
	}

	override def rcvPubTellers(vwpt: VWorldPublicTellers): Unit = {
		info0("OuterAppPumpSetup received pubTellers")
		val stageTeller = vwpt.getStageTeller.get
		val stageTickLover = vwpt.getStageTeller.get.asInstanceOf[CPStrongTeller[MsgToStatusSrc]]
		val pubStatTmpTellers = makePubStatTempBypassTeller_opt(vwpt).toList
		val tickLovers : List[CPStrongTeller[ReportingTickChance]] = List(stageTickLover) ++ pubStatTmpTellers
		setupOuterPumpActors(tickLovers)
		val nope = false
		if (nope) {
			setupDynamicStatusFlow(vwpt)
		}

		doAllExtraSetup(vwpt)
	}
	def setupDynamicStatusFlow(vwpt: VWorldPublicTellers): Unit = {
		val downstreamTeller = setupDownstreamActors_UnusedRightNow(vwpt.getOverlayTeller.get)
		val chanID = makeStampyRandyIdent()
		val wrong = downstreamTeller
		val toWhom: CPStrongTeller[SourceDataMsg] = ???
		val blankPolicy = new ReportingPolicy {}
		val chanOpenMsg = new ReportSrcOpen(chanID, toWhom, blankPolicy)

	}
}
// Unnecessary to use the Jobby approach here, but working through it anyway as a comparative exercise.
class OuterJobbyWrapper(outerLogic : OuterLogic) extends MsgJobLogic[VWorldPublicTellers] {
	// Differences here is that we get exception handling+logging, runtime type verification,
	// and actor wrapping for free, but we must also create the factory stuff below.
	// Note that we could also pass constructor parameters in via the factory, without Props hassles.
	override def processMsgUnsafe(msg : VWorldPublicTellers, slf : ActorRef, sndr : ActorRef,
								  actx : ActorContext) : Unit = {

		msg match {
			case vwpt : VWorldPublicTellers => 	{
				debug2("Received public-tellers-ready msg={} for outerLogic={}", msg, outerLogic)
				outerLogic.rcvPubTellers(vwpt)
			}
		}

	}
}
// Now we add the factories needed to construct our logic + actor instances.
// Question is:  When is this structure "better" than making an actor wrapper by hand, as done in DummyActorMaker, et al?
// Tentative answer:  When we expect to make a "bunch" of somewhat similar actors to do smaller tasks, then a coherent
// factory structure pays off more, as it yields stronger typing of a more regular set of objects, useful for monitoring
// and managing them.
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

// chose your fate
//   repeat
//   shuffle
//   exit
