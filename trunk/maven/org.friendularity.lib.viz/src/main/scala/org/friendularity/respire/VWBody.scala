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
import org.friendularity.rbody.DualBodyHelper

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
							mbsrc : ModelBlendingRobotServiceContext) : Unit = {
		val pmrc = getChrMgrCtx.getRenderCtx
		val hfm = getChrMgrCtx.getHumaFigMgr
		myHelper.finishDualBodInit(dualBodyID, mbsrc, pmrc, hfm, fullHumaCfg)
	}
	def processCharRq(vwcr: VWCharAdminRq, slfActr : ActorRef, localActorCtx : ActorContext): Unit = {
		vwcr match {
			case crchr : VWCreateCharRq => {
				info1("Processing create-char rq={}", crchr)
				createAndBindVWBody(crchr.dualBodyID, crchr.fullHumaCfg, crchr.myMBRoboSvcCtx)
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


