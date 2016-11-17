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
import java.lang.{Long => JLong, Integer => JInt}
import akka.actor.{ActorSystem, ActorRefFactory, ActorContext, ActorRef}
import com.jme3.math.{Quaternion, Vector3f, ColorRGBA}
import org.appdapter.core.name.FreeIdent
import org.appdapter.core.name.Ident
import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.api.fancy.FancyThingModelWriter
import org.cogchar.impl.thing.basic.BasicThingActionSpec
import org.cogchar.render.rendtest.GoodyTestMsgMaker
import org.friendularity.cpmsg.{ActorRefCPMsgTeller, CPStrongTeller, CPMsgTeller}

import org.friendularity.field.{StatusTickActorFactory, StatusTickDistributor, StatusTickScheduler, ReportingPolicy, ReportSrcOpen, SourceDataMsg, MsgToStatusSrc, ReportingTickChance, FieldActorFactory}
import org.friendularity.mjob.{MsgJobLogicFactory, MsgJobLogic}

import com.hp.hpl.jena.rdf.model.{Model => JenaModel, ModelFactory => JenaModelFactory, Literal}
import org.friendularity.vwimpl.{OverlayPage, IdentHlp, VWorldMasterFactory}

import scala.collection.immutable.HashMap

import org.friendularity.vwmsg.{VWBodyDangerYogaRq, KnownShapeCreateRqImpl, VWSCR_MeshyCmpnd, VWOverlayRq, VWSetupOvlBookRq, NavCmdImpl, NavCmdKeyClkBind, NavCmd, InnerNavCmds, VWorldPublicTellers, VWSCR_Node, VWBindCamNodeRq, VWCreateCamAndViewportRq, CamStateParams3D, CamState3D, ViewportDesc, ShapeManipRqImpl, SmooveManipEndingImpl, TransformParams3D, VWBodySkeletonDisplayToggle, VWBroadcastToAllBodies, VWClearAllShapes, VWStageResetToDefault, VWKeymapBinding_Medial, OrdinaryParams3D, VWSCR_Sphere, VWStageOpticsBasic, VWSCR_CellGrid, VWStageEmulateBonusContentAndCams, VWBodyLifeRq, VWRqTAWrapImpl, VWTARqTurtle, VWStatsViewMessage, VWStageSetupLighting, VWStageBackgroundColor, VWStageBackgroundSkybox}
import org.cogchar.impl.thing.fancy.ConcreteTVM
import org.cogchar.api.vworld.GoodyActionParamWriter
import org.cogchar.api.thing.SerTypedValueMap;
import org.cogchar.name.goody.GoodyNames

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
		if (extraSetupTasks.length > 0) {
			info3("{} is processing {} extra setup tasks: {}", this, extraSetupTasks.length : JInt, extraSetupTasks)
		}
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
trait GoodyTestMsgFun extends IdentHlp with TurtleSerHlp {
	import scala.collection.JavaConverters._
	private lazy val myRandomizer: JRandom = new JRandom

	def finallySendGoodyTstMsgs(goodyTeller : CPMsgTeller, flag_serToTurtle : Boolean): Unit = {
		val gtmm: GoodyTestMsgMaker = new GoodyTestMsgMaker
		val msgsJList = gtmm.makeGoodyCreationMsgs

		val msgsScbuf : List[BasicThingActionSpec] = msgsJList.asScala.toList // BTAS is known to be serializable

		for (actSpec <- msgsScbuf) {
			if (flag_serToTurtle) {
				val turtleTxt = serlzOneTAMsgToTurtleTxt(actSpec, myRandomizer)
				val turtleMsg = new VWTARqTurtle(turtleTxt)
				goodyTeller.tellCPMsg(turtleMsg)
			} else {
				getLogger.info("Sending java-serializable TA message: {}", actSpec)
				val vwMsgWrap = new VWRqTAWrapImpl(actSpec)
				goodyTeller.tellCPMsg(vwMsgWrap)
			}
		}
	}
}
trait FunWithShapes extends IdentHlp {
	def sendBigGridRq(shapeTeller : CPMsgTeller): Unit = {
		val rq_makeBigGrid = new VWSCR_CellGrid{}
		shapeTeller.tellCPMsg(rq_makeBigGrid)
	}

	def sendOvalRq_MAKE(shapeTeller : CPMsgTeller) : Ident = {
		val aqua : ColorRGBA = new ColorRGBA(0.1f,1.0f,0.5f, 0.65f)
		val sphereCol : ColorRGBA = aqua
		val spherePos = new Vector3f(-15.0f, 12.0f, 4.0f) // biggish dirigible
		val sphereRot = Quaternion.IDENTITY
		val sphereParams = new OrdinaryParams3D(spherePos, sphereRot, Vector3f.UNIT_XYZ, sphereCol)
		val msgPart_sphereDesc = new VWSCR_Sphere(9.0f, sphereParams)
		val knownSphereID_opt : Option[Ident] = Some(makeStampyRandyIdentAnon())
		val parentID_opt = None
		val msgPart_known = new KnownShapeCreateRqImpl(knownSphereID_opt, parentID_opt)
		val sphereCmpndReq = new VWSCR_MeshyCmpnd(msgPart_known, msgPart_sphereDesc)
		shapeTeller.tellCPMsg(sphereCmpndReq)
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
		val sphereManipMsg = new ShapeManipRqImpl(shapeID, endingManip, None)
		shapeTeller.tellCPMsg(sphereManipMsg)
	}
	def finallySendFunShapeRqs(shapeTeller : CPMsgTeller) : Unit = {
		sendBigGridRq(shapeTeller)
		val bigOvalID = sendOvalRq_MAKE(shapeTeller)
		val tgtXform = makeArbXform()
		sendShpSmooveRq(shapeTeller, bigOvalID, tgtXform, 40.0f)
	}

}

/**
 * Based off of {@link PatientSender_GoodyTest} but without the fun shapes. This is used
 * in {@link AppServiceHandles} to send Goody messages. 
 * (ben)[2016-10-04] 
 */
trait PatientSender_GoodyRouter extends OuterLogic {

	override def rcvPubTellers (vwpt : VWorldPublicTellers): Unit = {
		doAllExtraSetup(vwpt)
	}
}

trait PatientSender_GoodyTest extends OuterLogic with FunWithShapes with GoodyTestMsgFun
                                 {

	private var myStoredTellers_opt : Option[VWorldPublicTellers] = None

	private val useTurtleSerialization : Boolean = true
	private val testGoodyMsgsInternally : Boolean = false

	override def rcvPubTellers (vwpt : VWorldPublicTellers): Unit = {
		// This is the (outbound) notice we get back from boss, confirming system has started and these tellers are ready for biz.
		info1("Outer logic got public tellers: {}", vwpt)
		myStoredTellers_opt = Option(vwpt)
		val goodyTeller = vwpt.getGoodyDirectTeller
		if (goodyTeller.isDefined) {
			if (testGoodyMsgsInternally) {
				info1("Sending goody tst msgs to: {}", goodyTeller.get)
				finallySendGoodyTstMsgs(goodyTeller.get, useTurtleSerialization)
			} else {
				debug0("Skipping internal goody-msg test; note that goodies may instead be sent from ClientTestMsgSender, or other clients: AMQP | akka")
			}
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
			// We write it out long form here to make it more readable.
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
trait PatientSender_BonusStaging extends OuterLogic with OuterCamHelp with IdentHlp {
	override def rcvPubTellers(vwpt: VWorldPublicTellers): Unit = {
		val stageTeller = vwpt.getStageTeller.get

		val moveSpeed : Int = 60
        val pauseOnLostFocus = false
        val dragMouseToRotateCamera = true
        val location: Vector3f = new Vector3f(0f, 40f, 80f)
        val direction: Vector3f =  new Vector3f(0f, -0.3f, -1f)
		val opticsBasicRq = new VWStageOpticsBasic(location, direction, moveSpeed, pauseOnLostFocus, dragMouseToRotateCamera)
		stageTeller.tellCPMsg(opticsBasicRq)
		
		val darkBlue : ColorRGBA = new ColorRGBA(0f, 0.1f, 0.35f, 1f)
		val backgroundColorRequest = new VWStageBackgroundColor(darkBlue)
		stageTeller.tellCPMsg(backgroundColorRequest)

		sendSkyboxRq(stageTeller)
        
        val displayContentStatsOnScreen = false
        val displayFPSOnScreen = false
        val displayStatsOnScreenRequest = new VWStatsViewMessage(displayContentStatsOnScreen, displayFPSOnScreen)
        stageTeller.tellCPMsg(displayStatsOnScreenRequest)
		
		val mostlyWhite : ColorRGBA = new ColorRGBA(0.8f, 0.8f, 0.8f, 1f)
		val setupLightingRequest = new VWStageSetupLighting(mostlyWhite)
        stageTeller.tellCPMsg(setupLightingRequest)
    
		val emuBonusRq = new VWStageEmulateBonusContentAndCams()
	 	stageTeller.tellStrongCPMsg(emuBonusRq)

		setupStatusPumps(vwpt)

        setupFloorGoody(vwpt.getGoodyDirectTeller.get)

		val powerUserMode = false
		if (powerUserMode) {
			setupOverlayBook(vwpt)
		}

		setupKeysAndClicks(vwpt, powerUserMode)

        // (ben)[2016-10-04]: Removing for avatar release
//		sendExtraCameraRqs(stageTeller, vwpt.getShaperTeller.get)

		doAllExtraSetup(vwpt)
	}
	private def sendSkyboxRq(stageTeller : CPMsgTeller) : Unit = {
		// Ooops!
		// When we embed paths like this, we need to know where that resource is expected to be found, i.e. what bundle?
		val skyboxFolder : String = "textures/skybox/Sunny Ocean/";
		val northImagePath: String = skyboxFolder + "North.png"
		val eastImagePath: String = skyboxFolder + "East.png"
		val southImagePath: String = skyboxFolder + "South.png"
		val westImagePath: String = skyboxFolder + "West.png"
		val upImagePath: String = skyboxFolder + "Up.png"
		val downImagePath: String = skyboxFolder + "Down.png"
		val backgroundSkyBoxRequest = new VWStageBackgroundSkybox(northImagePath, eastImagePath, southImagePath,
			westImagePath, upImagePath, downImagePath)
		stageTeller.tellCPMsg(backgroundSkyBoxRequest)
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
    
  /**
   * TODO(ben)[2016-10-04]: Move to ttl sheet when we have the time.
   */
    def setupFloorGoody(messageTeller : CPMsgTeller) : Unit = {
        val  paramWriter : GoodyActionParamWriter = new GoodyActionParamWriter(new ConcreteTVM())
        // Virtual Floors *MUST* have a color and location
        paramWriter.putColor(0.5f, 0.5f, 0.5f, 1f)
        paramWriter.putLocation(0f, -0.5f, 0f)
        val  postedTStampMsec : Long = System.currentTimeMillis()
        val  valueMap : SerTypedValueMap = paramWriter.getValueMap()
        val actionInstanceID : Ident = new FreeIdent("urn:ftd:cogchar.org:2012:inst#action_create_default_virtual_floor")
        val entityID : Ident = new FreeIdent("urn:ftd:cogchar.org:2012:inst#default_virtual_floor")
        val entityTypeID : Ident = GoodyNames.TYPE_FLOOR
        val goodyActionVerbID : Ident = GoodyNames.ACTION_CREATE
        val sourceAgentID : Ident = new FreeIdent("urn:ftd:robosteps.org:2012:inst#agent_outer_logic_setup_floor_goody")
        
        val  thingActionSpec : BasicThingActionSpec = new BasicThingActionSpec(actionInstanceID,
                                                                         entityID,
                                                                         entityTypeID,
                                                                         goodyActionVerbID,
                                                                         sourceAgentID, valueMap,
                                                                         postedTStampMsec)
        val vwMsgWrap = new VWRqTAWrapImpl(thingActionSpec)
        messageTeller.tellCPMsg(vwMsgWrap)
	}

	def sendToggleSkelHilite(cadmTeller : CPMsgTeller) : Unit = {
		val innerBodyRq = new VWBodySkeletonDisplayToggle
		val brdcstRq = new VWBroadcastToAllBodies(innerBodyRq)
		cadmTeller.tellCPMsg(brdcstRq)
	}
	def sendBodyYoga(cadmTeller : CPMsgTeller) : Unit = {
		val innerBodyRq = new VWBodyDangerYogaRq
		val brdcstRq = new VWBroadcastToAllBodies(innerBodyRq)
		cadmTeller.tellCPMsg(brdcstRq)
	}

	// Cmds sent to navTeller

	def setupOverlayBook(vwpt: VWorldPublicTellers) : Unit = {

		val pages : List[OverlayPage] = NavPageDefs.pageList
		val ovlTeller = vwpt.getOverlayTeller.get

		val ovlSetupMsg = new VWSetupOvlBookRq(pages)

		ovlTeller.tellStrongCPMsg(ovlSetupMsg)

		val outerKeysWidg = new OuterBindNavCmdKeys{}
		val kcmdReg = new VWKeymapBinding_Medial(outerKeysWidg.navCmdKeyBindMap, vwpt)
		val stageTeller = vwpt.getStageTeller.get
		stageTeller.tellCPMsg(kcmdReg)
	}

	def setupStatusPumps(vwpt: VWorldPublicTellers) : Unit = {}

	private def setupPowerUserKeys(vwpt: VWorldPublicTellers): Unit = {
		// Define callback functions to be invoked in response to keyboard down stroke events.
		val funcStgRst = (pt: VWorldPublicTellers) => {
			sendStageReset(pt.getStageTeller.get)
		}
		val funcClrShps = (pt: VWorldPublicTellers) => {
			sendClearShaps(pt.getShaperTeller.get)
		}
		val funcBodyYogaTest = (pt : VWorldPublicTellers) => {
			sendBodyYoga(pt.getCharAdminTeller.get)
		}

		val inlineMap: Map[String, Function1[VWorldPublicTellers, Unit]] = Map(
			"K" -> funcStgRst,
			"L" -> funcClrShps,
			"F3" -> funcBodyYogaTest)

		// val typedMap =  HashMap(inlineMap)
		info1("inlineKeymap={}", inlineMap) // , typedMap)
		val regMsg = new VWKeymapBinding_Medial(inlineMap, vwpt)
		val stageTeller = vwpt.getStageTeller.get
		stageTeller.tellCPMsg(regMsg)

	}
	def setupKeysAndClicks(vwpt: VWorldPublicTellers, powerUserMode : Boolean) : Unit = {

		val stageTeller = vwpt.getStageTeller.get

		// sendClearKeysAndClicks(stageTeller)

		if (powerUserMode) {
			setupPowerUserKeys(vwpt)
		}
		// Define callback functions to be invoked in response to keyboard down stroke events.
		val funcSkelHiliteToggle = (pt : VWorldPublicTellers) => {sendToggleSkelHilite(pt.getCharAdminTeller.get)}

		val nextMap = Map("F2" -> funcSkelHiliteToggle)
		val regMsg2 = new VWKeymapBinding_Medial(nextMap, vwpt)
		stageTeller.tellCPMsg(regMsg2)

	}
	def sendExtraCameraRqs(stageTeller : CPMsgTeller, spcTeller : CPMsgTeller): Unit = {
		val vpd = new ViewportDesc(0.2f, 0.4f, 0.15f, 0.30f, Some(ColorRGBA.DarkGray))
		val cpv = new Vector3f(-70.0f, 5.0f, -3.0f)
		val pdir = new Vector3f(1.0f, 0.0f, 0.0f)
		val cst = new CamStateParams3D(cpv, pdir)

		val camGuideNodeID : Ident = makeAndBindExtraCam(stageTeller, spcTeller, "extra", cst, vpd)
		/*
		val camID : Ident = makeStampyRandyIdentAnon
		val makeCamRq = new VWCreateCamAndViewportRq(camID, cst, vpd)
		stageTeller.tellCPMsg(makeCamRq)

		val camGuideNodeID : Ident = makeStampyRandyIdentAnon
		info1("Requesting cam-guide node at ID={}", camGuideNodeID)
		val makeGuideNodeRQ = new VWSCR_Node(camGuideNodeID, None)
		spcTeller.tellCPMsg(makeGuideNodeRQ)

		val guideIsParent = true
		val camGuideBindRq = new VWBindCamNodeRq(camID, guideIsParent, spcTeller, camGuideNodeID)
		stageTeller.tellCPMsg(camGuideBindRq)
		*/
		val guideTgtPos = new Vector3f(-1.0f, 5.0f, 3.0f)
		val rotAngles = Array(45.0f, -45.0f, 15.0f)
		val guideTgtRot = new Quaternion(rotAngles)
		val guideTgtScale = Vector3f.UNIT_XYZ
		val guideTgtXform = new TransformParams3D(guideTgtPos, guideTgtRot, guideTgtScale)

		sendGuidedCamMoveRq(spcTeller, camGuideNodeID, guideTgtXform, Some(60.0f), None)
		// val endingManip = new SmooveManipEndingImpl(guideTgtXform, 60.0f)
		// val guideManipMsg = new ShapeManipRqImpl(camGuideNodeID, endingManip)
		// spcTeller.tellCPMsg(guideManipMsg)

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
		val chanID = makeStampyRandyIdentAnon()
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
