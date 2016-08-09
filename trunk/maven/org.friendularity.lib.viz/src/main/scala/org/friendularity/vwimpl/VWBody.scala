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

package org.friendularity.vwimpl

import akka.actor.{Actor, ActorContext, ActorRef}
import org.appdapter.core.name.Ident
import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.api.humanoid.HumanoidFigureConfig
import org.cogchar.bind.mio.robot.svc.ModelBlendingRobotServiceContext
import org.cogchar.render.model.humanoid.HumanoidFigureManager
import org.cogchar.render.sys.context.PhysicalModularRenderContext
import org.cogchar.render.sys.registry.RenderRegistryClient
import org.friendularity.cpmsg.{ActorRefCPMsgTeller, CPStrongTeller}

import org.friendularity.rbody.{DualBodyHelper, DualBodyRecord}
import org.friendularity.vwmsg.{ManipDesc, VWBodyManipRq, VWBroadcastToAllBodies, VWBodySkeletonDisplayToggle, VWBodyLifeRq, VWBodyMakeRq,  VWBodyNotice, VWBodyRq}

/**
  * Created by Stub22 on 6/6/2016.
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

	// This var only exists to support broadcasting msgs to all bodies

	def createAndBindVWBody(dualBodyID: Ident,  fullHumaCfg : HumanoidFigureConfig,
							mbsrc_opt : Option[ModelBlendingRobotServiceContext]) : DualBodyRecord = {
		val pmrc = getChrMgrCtx.getRenderCtx
		val hfm = getChrMgrCtx.getHumaFigMgr
		val dbr = myHelper.finishDualBodInit(dualBodyID, mbsrc_opt, pmrc, hfm, fullHumaCfg)
		dbr
	}
	private var myBodyTellersForBrdcst :Set[CPStrongTeller[VWBodyRq]] = Set()
	def processCharRq(vwcr: VWBodyLifeRq, slfActr : ActorRef, localActorCtx : ActorContext): Unit = {
		vwcr match {
			case crchr: VWBodyMakeRq => {
				info1("Processing create-char rq={}", crchr)
				val dbr: DualBodyRecord = createAndBindVWBody(crchr.dualBodyID, crchr.fullHumaCfg, crchr.myMBRoboSvcCtx_opt)
				info1("Finished connecting dual body, now what kind of notices do we want to send?  dbr={}", dbr)
				val actorName = "bdActr_" + crchr.dualBodyID.getLocalName
				val bodyActor = VWorldActorFactoryFuncs.makeVWBodyActor(localActorCtx, actorName, dbr)
				val bodyTeller = new ActorRefCPMsgTeller[VWBodyRq](bodyActor)
				myBodyTellersForBrdcst += bodyTeller
				val bodyNotice = new VWBodyNotice {
					override def getBodyTeller: CPStrongTeller[VWBodyRq] = bodyTeller
				}
				crchr.answerTeller.tellStrongCPMsg(bodyNotice)

			}
			case brdcst: VWBroadcastToAllBodies => {
				val msgToBrdcst = brdcst.bodyRQ
				broadcastBodyRq(msgToBrdcst)
			}
		}
	}
	protected def broadcastBodyRq(bodyRq : VWBodyRq) : Unit = {
		for (bt <- myBodyTellersForBrdcst) {
			bt.tellStrongCPMsg(bodyRq)
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

trait VWBodyLogic extends FullEnqHlp with VarargsLogging {
	protected def getBodyRec : DualBodyRecord
	override protected def getRRC: RenderRegistryClient = getBodyRec.rrc

	var myBodyManipRqCnt : Int = 0
	protected def getManipDumpPeriod : Int = 7

	protected def processBodyRq(bodyRq : VWBodyRq, slfActr : ActorRef, localActorCtx : ActorContext): Unit = {
		val bodyRec = getBodyRec


		bodyRq match {
			// case moverq : VWBodyMoveRq => {
			//	info1("Moving body according to moveRq={}", moverq)
			//	bodyRec.moveVWBody_usingEntity(moverq.xPos, moverq.yPos, moverq.zPos)
			//}
			case toggleSkelHilite : VWBodySkeletonDisplayToggle => {
				info1("Toggling skeleton hilite for body={}", bodyRec)
				val fig = bodyRec.humaFig
				val func = () => {fig.toggleDebugSkeleton_onSceneThread}
				enqueueJmeCallable(func)
			}
			case manipWrap : VWBodyManipRq => {
				myBodyManipRqCnt += 1
				if ((myBodyManipRqCnt % getManipDumpPeriod) == 0) {
					info3("Received {}th bodyManipRq {} for bodyID={}", myBodyManipRqCnt : Integer, bodyRq, bodyRec.dualBodyID)
				} else {
					debug3("Received {}th bodyManipRq {} for bodyID={}", myBodyManipRqCnt : Integer, bodyRq, bodyRec.dualBodyID)
				}
				val manipGuts : ManipDesc = manipWrap.manipGuts
				bodyRec.applyManipDesc(manipGuts, this)

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