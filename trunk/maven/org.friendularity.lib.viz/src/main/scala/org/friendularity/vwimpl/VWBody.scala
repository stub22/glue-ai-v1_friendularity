package org.friendularity.vwimpl

import akka.actor.{Actor, ActorContext, ActorRef}
import org.appdapter.core.name.Ident
import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.api.humanoid.HumanoidFigureConfig
import org.cogchar.bind.mio.robot.svc.ModelBlendingRobotServiceContext
import org.cogchar.render.model.humanoid.HumanoidFigureManager
import org.cogchar.render.sys.context.PhysicalModularRenderContext
import org.friendularity.cpump.{ActorRefCPMsgTeller, CPStrongTeller}
import org.friendularity.rbody.{DualBodyHelper, DualBodyRecord}
import org.friendularity.vwmsg.{VWBodyLifeRq, VWBodyMakeRq, VWBodyMoveRq, VWBodyNotice, VWBodyRq}

/**
  * Created by Owner on 6/6/2016.
  */

trait VWCharMgrCtx {
	def getRenderCtx : PhysicalModularRenderContext
	def getHumaFigMgr :  HumanoidFigureManager
}
class VWCharMgrCtxImpl(rendCtx : PhysicalModularRenderContext) extends VWCharMgrCtx {
	lazy val myHumaFigMgr = new HumanoidFigureManager

	override def getRenderCtx : PhysicalModularRenderContext = rendCtx
	override def getHumaFigMgr :  HumanoidFigureManager = myHumaFigMgr
}

trait VWCharMgrJobLogic extends VarargsLogging {
	protected def getChrMgrCtx : VWCharMgrCtx

	lazy private val myHelper = new DualBodyHelper
	def createAndBindVWBody(dualBodyID: Ident,  fullHumaCfg : HumanoidFigureConfig,
							mbsrc_opt : Option[ModelBlendingRobotServiceContext]) : DualBodyRecord = {
		val pmrc = getChrMgrCtx.getRenderCtx
		val hfm = getChrMgrCtx.getHumaFigMgr
		myHelper.finishDualBodInit(dualBodyID, mbsrc_opt, pmrc, hfm, fullHumaCfg)
	}
	def processCharRq(vwcr: VWBodyLifeRq, slfActr : ActorRef, localActorCtx : ActorContext): Unit = {
		vwcr match {
			case crchr : VWBodyMakeRq => {
				info1("Processing create-char rq={}", crchr)
				val dbr : DualBodyRecord = createAndBindVWBody(crchr.dualBodyID, crchr.fullHumaCfg, crchr.myMBRoboSvcCtx_opt)
				info1("Finished connecting dual body, now what kind of notices do we want to send?  dbr={}", dbr)
				val actorName = "bdActr_" + crchr.dualBodyID.getLocalName
				val bodyActor = VWorldActorFactoryFuncs.makeVWBodyActor(localActorCtx, actorName, dbr)
				val bodyTeller = new ActorRefCPMsgTeller[VWBodyRq](bodyActor)
				val bodyNotice = new VWBodyNotice {
					override def getBodyTeller: CPStrongTeller[VWBodyRq] = bodyTeller
				}
				crchr.answerTeller.tellStrongCPMsg(bodyNotice)
			}
		}
	}
}

class VWCharMgrActor(myBodyCtx : VWCharMgrCtx) extends Actor with VWCharMgrJobLogic {
	override protected def getChrMgrCtx : VWCharMgrCtx = myBodyCtx
	def receive = {
		case vwrq: VWBodyLifeRq => {
			processCharRq(vwrq, self, context)
		}
	}
}



trait VWBodyLogic extends VarargsLogging {
	protected def getBodyRec : DualBodyRecord

	protected def processBodyRq(bodyRq : VWBodyRq, slfActr : ActorRef, localActorCtx : ActorContext): Unit = {
		info2("Received bodyRq {} for bodyID={}", bodyRq, getBodyRec.dualBodyID)
		bodyRq match {
			case moverq : VWBodyMoveRq => {
				info1("Moving body according to moveRq={}", moverq)
				getBodyRec.moveVWBody_usingEntity(moverq.xPos, moverq.yPos, moverq.zPos)
			}
		}
	}
}

class VWBodyActor(dualBodyRec : DualBodyRecord) extends Actor with VWBodyLogic {
	override protected def getBodyRec: DualBodyRecord = dualBodyRec
	def receive = {
		case vwbrq: VWBodyRq => {
			processBodyRq(vwbrq, self, context)
		}
	}
}