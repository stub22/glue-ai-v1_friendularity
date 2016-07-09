package org.friendularity.navui

import java.util.{Random => JRandom}
import java.lang.{Long => JLong}
import akka.actor.{ActorRefFactory, ActorContext, ActorRef}
import com.jme3.math.{Quaternion, Vector3f, ColorRGBA}
import org.appdapter.core.name.Ident
import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.api.fancy.FancyThingModelWriter
import org.cogchar.render.rendtest.GoodyTestMsgMaker
import org.friendularity.cpump.{CPumpMsg, ActorRefCPMsgTeller, CPStrongTeller, CPMsgTeller}
import org.friendularity.mjob.{MsgJobLogicFactory, MsgJobLogic}

import com.hp.hpl.jena.rdf.model.{Model => JenaModel, ModelFactory => JenaModelFactory, Literal}
import org.friendularity.vwimpl.{IdentHlp, VWorldMasterFactory}

import scala.collection.immutable.HashMap

import org.friendularity.vwmsg.{NavCmdImpl, NavCmdKeyClkBind, NavCmd, InnerNavCmds, VWorldPublicTellers, VWSCR_Node, VWBindCamNodeRq, VWCreateCamAndViewportRq, CamStateParams3D, CamState3D, ViewportDesc, ShapeManipRqImpl, SmooveManipEndingImpl, TransformParams3D, VWBodySkeletonDisplayToggle, VWBroadcastToAllBodies, VWClearAllShapes, VWStageResetToDefault, VWKeymapBinding_Medial, OrdinaryParams3D, VWSCR_Sphere, VWStageOpticsBasic, VWSCR_CellGrid, VWStageEmulateBonusContentAndCams, VWBodyLifeRq, VWGoodyRqTAS, VWGoodyRqTurtle}


/**
  * Limits
  */
trait OuterLogic extends VarargsLogging {
	def rcvPubTellers (vwpt : VWorldPublicTellers): Unit
}
trait PatientSender_GoodyTest extends OuterLogic with IdentHlp {
	import scala.collection.JavaConverters._

	lazy val myRandomizer: JRandom = new JRandom

	def finallySendFunShapeRqs(shapeTeller : CPMsgTeller) : Unit = {
		val rq_makeBigGrid = new VWSCR_CellGrid{}
		val sphereCol = new ColorRGBA(0.1f,1.0f,0.5f, 0.65f)
		val spherePos = new Vector3f(-15.0f, 12.0f, 4.0f)
		val sphereRot = Quaternion.IDENTITY
		val sphereParams = new OrdinaryParams3D(spherePos, sphereRot, Vector3f.UNIT_XYZ, sphereCol)
		val knownSphereID_opt : Option[Ident] = Some(makeStampyRandyIdent())
		val rq_makeSphere = new VWSCR_Sphere(9.0f, sphereParams, knownSphereID_opt)
		shapeTeller.tellCPMsg(rq_makeSphere)
		shapeTeller.tellCPMsg(rq_makeBigGrid)

		val tgtPos = new Vector3f(-25.0f, 45.0f, 6.0f)
		val tgtRot = Quaternion.IDENTITY
		val tgtScale = new Vector3f(1.0f, 0.5f, 4.0f)
		val tgtXform = new TransformParams3D(tgtPos, tgtRot, tgtScale)
		val endingManip = new SmooveManipEndingImpl(tgtXform, 40.0f)
		val sphereManipMsg = new ShapeManipRqImpl(knownSphereID_opt.get, endingManip)
		shapeTeller.tellCPMsg(sphereManipMsg)

		// chose your fate
		//   repeat
		//   reshuffle
		//   exit
	}

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

	}
}

trait PatientForwarder_CharAdminTest extends OuterLogic {
	private var myStoredTellers_opt : Option[VWorldPublicTellers] = None
	private val myPendingCharAdminRqs = new scala.collection.mutable.ListBuffer[VWBodyLifeRq]() //  = Nil // Any inbound messages we have not gotten to yet.

	override def rcvPubTellers(vwpt: VWorldPublicTellers): Unit = {
		myStoredTellers_opt = Option(vwpt)
		propagateMessages(myStoredTellers_opt)
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

		setupKeysAndClicks(vwpt)

		sendExtraCameraRqs(stageTeller, vwpt.getShaperTeller.get)

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

// Unnecessary to use the Jobby approach here, but working through it anyway as a comparative exercise.
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
