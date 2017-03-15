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

package org.friendularity.vw.impl.bdy

import java.lang.{Integer => JInt, Long => JLong}
import java.util.{List => JList}

import akka.actor.{Actor, ActorContext, ActorRef}
import org.appdapter.core.name.Ident
import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.api.humanoid.HumanoidFigureConfig
import org.cogchar.bind.mio.robot.client.RobotAnimClient.BuiltinAnimKind
import org.cogchar.bind.mio.robot.client.{RobotAnimClient, RobotAnimContext}
import org.cogchar.bind.mio.robot.model.ModelRobot
import org.cogchar.bind.mio.robot.svc.ModelBlendingRobotServiceContext
import org.cogchar.render.model.humanoid.HumanoidFigureManager
import org.cogchar.render.sys.context.PhysicalModularRenderContext
import org.cogchar.render.sys.registry.RenderRegistryClient
import org.friendularity.infra.cpmsg.{ActorRefCPMsgTeller, CPStrongTeller}
import org.friendularity.rbody.{DualBodyHelper, DualBodyRecord, EnhRobotAnimContext}
import org.friendularity.vw.impl.sys.VWorldActorFactoryFuncs
import org.friendularity.vw.impl.tsk.FullJmeEnqHlp
import org.friendularity.vw.mprt.manip.ManipDesc
import org.friendularity.vw.msg.bdy.{VWBodyDangerYogaRq, VWBodyFindRq, VWBodyLifeRq, VWBodyManipRq, VWBodyNoticeImpl, VWBodyRq, VWBodySkeletonDisplayToggle, VWBroadcastToAllBodies}
import org.friendularity.vw.msg.intrn.VWBodyMakeRq
import org.mechio.api.animation.{Animation, Channel}

import scala.collection.mutable

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
		val dbr : DualBodyRecord = myHelper.finishDualBodInit(dualBodyID, mbsrc_opt, pmrc, hfm, fullHumaCfg)
		dbr
	}
	private val myBTlrsByBodyID = new mutable.HashMap[Ident, CPStrongTeller[VWBodyRq]]()
	private var myBodyTellersForBrdcst :Set[CPStrongTeller[VWBodyRq]] = Set()
	def processCharRq(vwcr: VWBodyLifeRq, slfActr : ActorRef, localActorCtx : ActorContext): Unit = {
		vwcr match {
			case mkVWBody: VWBodyMakeRq => {
				val vwBodyID = mkVWBody.dualBodyID
				info2("Processing create-char rq for bodyID={}, rq={}", vwBodyID, mkVWBody)
				val dbr: DualBodyRecord =
					createAndBindVWBody(mkVWBody.dualBodyID, mkVWBody.fullHumaCfg, mkVWBody.myMBRoboSvcCtx_opt)
				info1("Finished connecting dual body, now what kind of notices do we want to send?  dbr={}", dbr)
				val actorName = "bdActr_" + vwBodyID.getLocalName
				val bodyActor = VWorldActorFactoryFuncs.makeVWBodyActor(localActorCtx, actorName, dbr)
				val bodyTeller = new ActorRefCPMsgTeller[VWBodyRq](bodyActor)
				myBodyTellersForBrdcst += bodyTeller
				myBTlrsByBodyID.put(vwBodyID, bodyTeller)
				val bodyNotice = new VWBodyNoticeImpl(vwBodyID, Some(bodyTeller))
				mkVWBody.answerTeller.tellStrongCPMsg(bodyNotice)
			}
			case bfrq: VWBodyFindRq => {
				val vwBodyID = bfrq.dualBodyID
				val bodyTeller_opt = myBTlrsByBodyID.get(vwBodyID)
				val bodyNotice = new VWBodyNoticeImpl(vwBodyID, bodyTeller_opt)
				bfrq.answerTeller.tellStrongCPMsg(bodyNotice)
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

trait VWBodyLogic extends FullJmeEnqHlp with VarargsLogging {
	protected def getBodyRec : DualBodyRecord
	override protected def getRRC: RenderRegistryClient = getBodyRec.rrc

	var myBodyManipRqCnt : Int = 0
	protected def getManipDumpPeriod : Int = 7

	import scala.collection.JavaConverters._

	protected def processBodyRq(bodyRq : VWBodyRq, slfActr : ActorRef, localActorCtx : ActorContext): Unit = {
		val bodyRec : DualBodyRecord = getBodyRec

		bodyRq match {
			case toggleSkelHilite : VWBodySkeletonDisplayToggle => {
				info1("Toggling skeleton hilite for body={}", bodyRec)
				val fig = bodyRec.humaFig
				val func = () => {fig.toggleDebugSkeleton_onSceneThread}
				enqueueJmeCallable(func)
			}
			case dangerYogaRq : VWBodyDangerYogaRq => {
				info1("Starting danger-yoga motion [expected to work ONLY in OSGi apps as of 2016-09-09] for body={}", bodyRec)
				val ranimCtx_opt = bodyRec.myRobotAnimCtx_opt
				if (ranimCtx_opt.isDefined) {
					val ranimCtx : RobotAnimContext = ranimCtx_opt.get
					val enhRanimCtx : EnhRobotAnimContext = ranimCtx.asInstanceOf[EnhRobotAnimContext]
					val animKind = BuiltinAnimKind.BAK_DANGER_YOGA
					// info1("Calling playBuiltinAnimNow on animCtx={}", ranimCtx)
					// ranimCtx.playBuiltinAnimNow(animKind)
					val modelRobot : ModelRobot = enhRanimCtx.getRobot
					val animCli : RobotAnimClient = enhRanimCtx.getAnimClient
					val builtinAnim : Animation = animCli.makeBuiltinAnim(animKind, modelRobot);
					val chanList : JList[Channel] = builtinAnim.getChannels
					info2("Made builtin anim of length={} with channels={}", builtinAnim.getLength : JLong, chanList)
					for (c <- chanList.asScala.toList) {
						val chan : Channel = c
						info2("Chan id={} has name={}", chan.getId, chan.getName)
					}
					info1("Anim.toString = {}", builtinAnim)
					val job = enhRanimCtx.startFullAnimationNow(builtinAnim)
					info1("Started animation, job={}", job)
				}
			}
			case manipWrap : VWBodyManipRq => {
				myBodyManipRqCnt += 1
				if ((myBodyManipRqCnt % getManipDumpPeriod) == 0) {
					info3("Received {}th bodyManipRq {} for bodyID={}", myBodyManipRqCnt : Integer, bodyRq, bodyRec.dualBodyID)
				} else {
					debug3("Received {}th bodyManipRq {} for bodyID={}", myBodyManipRqCnt : Integer, bodyRq, bodyRec.dualBodyID)
				}
				val manipGuts : ManipDesc = manipWrap.manipGuts
				bodyRec.applyManipDesc(manipGuts, this, None)

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