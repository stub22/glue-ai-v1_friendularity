package org.friendularity.respire

import akka.actor.{ActorContext, ActorRef, Actor}
import org.appdapter.core.name.Ident
import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.api.humanoid.HumanoidFigureConfig
import org.cogchar.bind.mio.robot.svc.ModelBlendingRobotServiceContext
import org.cogchar.render.app.humanoid.HumanoidRenderContext
import org.cogchar.render.goody.basic.BasicGoodyCtx
import org.cogchar.render.model.humanoid.HumanoidFigureManager
import org.cogchar.render.sys.context.PhysicalModularRenderContext
import org.friendularity.cpump.{CPStrongTeller, ActorRefCPMsgTeller}
import org.friendularity.rbody.{DualBodyRecord, DualBodyHelper}

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
							mbsrc : ModelBlendingRobotServiceContext) : DualBodyRecord = {
		val pmrc = getChrMgrCtx.getRenderCtx
		val hfm = getChrMgrCtx.getHumaFigMgr
		myHelper.finishDualBodInit(dualBodyID, mbsrc, pmrc, hfm, fullHumaCfg)
	}
	def processCharRq(vwcr: VWCharAdminRq, slfActr : ActorRef, localActorCtx : ActorContext): Unit = {
		vwcr match {
			case crchr : VWCreateCharRq => {
				info1("Processing create-char rq={}", crchr)
				val dbr : DualBodyRecord = createAndBindVWBody(crchr.dualBodyID, crchr.fullHumaCfg, crchr.myMBRoboSvcCtx)
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
		case vwrq: VWCharAdminRq => {
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
			}
		}
	}
}

class VWBodyActor(dualBodyRec : DualBodyRecord) extends Actor with VWBodyLogic {
	protected def getBodyRec: DualBodyRecord = dualBodyRec
	def receive = {
		case vwbrq: VWBodyRq => {
			processBodyRq(vwbrq, self, context)
		}
	}
}