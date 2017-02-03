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

package org.friendularity.vw.impl.ta

import java.lang.{Float => JFloat, Integer => JInt, Long => JLong}

import akka.actor.{ActorRef, Props}
import com.jme3.math.{ColorRGBA, Quaternion, Vector3f}
import org.appdapter.core.name.{FreeIdent, Ident}
import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.api.thing.{ThingActionSpec, TypedValueMap}
import org.cogchar.name.cinema.LightsCameraAN
import org.cogchar.name.goody.GoodyNames
import org.cogchar.render.app.entity.GoodyActionExtractor
import org.friendularity.akact.{FrienduActor, KnowsAkkaSys}
import org.friendularity.cpmsg.{ActorRefCPMsgTeller, CPMsgTeller, CPStrongTeller}
import org.friendularity.util.IdentHlp
import org.friendularity.vw.cli.cam.OuterCamHelp
import org.friendularity.vw.mprt.manip.{CamState3D, CamStateParams3D, MakesManipDesc, MakesTransform3D, ManipStatusMsg, MaybeTransform3D, PartialTransform3D}
import org.friendularity.vw.msg.bdy.{VWBodyFindRq, VWBodyLifeRq, VWBodyManipRq, VWBodyNotice, VWBodyRq}
import org.friendularity.vw.msg.shp.deep.{VWShapeAttachRq, VWShapeDetachRq}
import org.friendularity.vw.msg.stg.ViewportDesc
import org.friendularity.vwmsg.{VWRqTAWrapper, VWorldPublicTellers}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
/**
  * Created by Stub22 on 8/11/2016.
  */


trait VWThingActReqRouterLogic extends VWBodyTARouterLogic with CamTARouterLogic with MakesTransform3D {

	lazy private val myBodyMedialRndzvs = new VWBodyMedialRendezvous {
		override def getCharAdminTeller: CPStrongTeller[VWBodyLifeRq] = getVWPubTellers.getCharAdminTeller.get
	}
	override protected def getMedialRendezvous :  VWBodyMedialRendezvous  = myBodyMedialRndzvs
	def routeRq(vwtarq: VWRqTAWrapper, whoDat : ActorRef) : Unit = {
		val tellers = getVWPubTellers
		debug1("Router handling TARq in binary format: {}, and wisely using vwpt-tellers...", vwtarq)

		val taSpec : ThingActionSpec = vwtarq.getActionSpec
		val gax: GoodyActionExtractor = new GoodyActionExtractor(taSpec)
		val targetTypeID = gax.getType

		// Handle TA messages for 3 primary kinds of entity:  Avatar-body, VWCamera, Goody.
		// First two handle primarily:
		// * Movements, both smooth and abrupt, along axes of position, rotation, scale.
		// * Creation, attachment, detach
		if (targetTypeID.equals(GoodyNames.TYPE_AVATAR)) {
			handleBodyTA(taSpec, gax, whoDat)
		} else if (targetTypeID.equals(GoodyNames.TYPE_CAMERA)) {
			handleCameraTA(taSpec, gax, whoDat)
		} else {
			// Other messages are presumed to refer to goodies, and are forwarded to a
			// dedicated actor that translates and directs them.   These may involve
			// create/destroy, attach/detach, movement (smooth or abrupt), goody-state changes,
			// color+transparency changes.
			// As of 2016-08-28, this message eventually gets handled by
			// VWGoodyJobLogic (in VWGoody.scala), which in turn passes control to an instance of
			// BetterBGC.   We use the old cogchar method:
			//   o.c.render.goody.basic.BasicGoodyCtxImpl.consumeAction(actSpec)
			// However, in the case of CREATE operations, the behavior is overridden in
			// BetterBGC.createByAction.
			// That object connection was attached during startup in
			// VWorldBossLogic.completeBossSetupAndPublish (in VWBoss.scala).

			val directLegacyGoodyTeller = getVWPubTellers.getGoodyDirectTeller
			directLegacyGoodyTeller.get.tellStrongCPMsg(vwtarq)
		}
	}

}
class VWThingActReqRouterActor(routerLogic : VWThingActReqRouterLogic) extends FrienduActor {
	override def receive = {
		case vwgrq: VWRqTAWrapper => {
			routerLogic.routeRq(vwgrq, self)
		}
		case vwbn: VWBodyNotice => {
			routerLogic.receiveBodyNotice(vwbn)
		}
		case manipStat : ManipStatusMsg => {
			routerLogic.receiveManipStatus(manipStat)
		}
		case other => {
			getLogger().warn("ThingActReqRouterActor received unexpected message: {}", other)
		}
	}
}
// Makes an actor and teller-wrapper that can receive direct VW-TA request instances, from any source.
trait MakesVWTAReqRouterTeller extends KnowsAkkaSys with VarargsLogging {
	protected def makeBinSerRoutingTeller(vwpt : VWorldPublicTellers) : CPStrongTeller[VWRqTAWrapper] = {
		val akkaSys = getAkkaSys
		val routerLogic = new VWThingActReqRouterLogic {
			val myVWPT : VWorldPublicTellers = vwpt
			override def getVWPubTellers : VWorldPublicTellers = myVWPT
		}
		info1("Made routerLogic={}", routerLogic)
		val routerActorProps = Props(classOf[VWThingActReqRouterActor], routerLogic)
		val routerActorRef : ActorRef = akkaSys.actorOf(routerActorProps, "taBinSerRouterActr")
		info1("Made routerActorRef={}", routerActorRef)
		val routerTeller = new ActorRefCPMsgTeller[VWRqTAWrapper](routerActorRef)
		routerTeller
	}

}