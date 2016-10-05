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

import akka.actor.{ActorRef, Props}
import com.jme3.math.{ColorRGBA, Quaternion, Vector3f}
import org.appdapter.core.name.Ident
import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.api.thing.{TypedValueMap, ThingActionSpec}
import org.cogchar.name.goody.GoodyNames
import org.cogchar.render.app.entity.GoodyActionExtractor
import org.friendularity.akact.{KnowsAkkaSys, FrienduActor}
import org.friendularity.cpmsg.{CPMsgTeller, ActorRefCPMsgTeller, CPStrongTeller}
import org.friendularity.navui.OuterCamHelp
import org.friendularity.vwmsg.{TransformParams3D, CamStateParams3D, ViewportDesc, CamState3D, SmooveManipEndingImpl, VWBodyManipRq, AbruptManipAbsImpl, SmooveManipGutsImpl, ManipDesc, Transform3D, MakesTransform3D, PartialTransform3D, MaybeTransform3D, VWBodyFindRq, VWBodyLifeRq, VWBodyRq, VWBodyNotice, VWorldPublicTellers, VWRqTAWrapper}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import java.lang.{Integer => JInt, Long => JLong, Float => JFloat}
/**
  * Created by Stub22 on 8/11/2016.
  */

trait TARqExtractorHelp {
	import scala.collection.JavaConverters._
	def extractXform (tvm : TypedValueMap, gax : GoodyActionExtractor) : MaybeTransform3D = {
		val allKeys : Set[Ident] = tvm.iterateKeys().asScala.toSet
		// We assume that presence or absence of a single coordinate indicates that part of
		// each transform: Location, Rotation, Scale.
		// Location
		val loc_opt : Option[Vector3f] = {
			if(allKeys.contains(GoodyNames.LOCATION_X))
				Option(gax.getLocationVec3f)
			else None
		}
		// Rotation
		val rot_opt : Option[Quaternion] = {
			if(allKeys.contains(GoodyNames.ROTATION_MAG_DEG))
				Option(gax.getRotationQuaternion)
			else None
		}
		// Scale
		val scl_opt : Option[Vector3f] = {
			if(allKeys.contains(GoodyNames.SCALE_X))
				Option(gax.getScaleVec3f)
			else {
				if (allKeys.contains(GoodyNames.SCALE_UNIFORM)) {
					val scaleUni : Float = gax.getScaleUniform
					Some(new Vector3f(scaleUni, scaleUni, scaleUni))
				} else None
			}
		}
		// "Partial" here indicates that each of the 3 pieces is optional.
		new PartialTransform3D(loc_opt, rot_opt, scl_opt)
	}
	def extractDuration(tvm : TypedValueMap) : Option[JFloat] = Option(tvm.getAsFloat(GoodyNames.TRAVEL_TIME))

	def extractColor(gax : GoodyActionExtractor) : Option[ColorRGBA] = Option(gax.getColor)
}
trait VWBodyMedialRendezvous extends VarargsLogging {
	def getCharAdminTeller : CPStrongTeller[VWBodyLifeRq]

	val myBodyTlrsByID = new mutable.HashMap[Ident, CPStrongTeller[VWBodyRq]]
	val myFailedBodyIDs = new mutable.HashSet[Ident]
	val myPendingBodyQueues = new mutable.HashMap[Ident,ListBuffer[VWBodyRq]]
	def noticeBody(bn : VWBodyNotice) : Unit = {
		val bodyID : Ident = bn.getVWBodyID
		val btlr_opt : Option[CPStrongTeller[VWBodyRq]] = bn.getBodyTeller_opt
		if (btlr_opt.isDefined) {
			val btlr = btlr_opt.get
			info2("Registering TA-router path for bodyID={}, teller={}", bodyID, btlr)
			myBodyTlrsByID.put(bodyID, btlr)
			val pendingQueue_opt = myPendingBodyQueues.get(bodyID)
			pendingQueue_opt.map(pq => {
				myPendingBodyQueues.remove(bodyID)
				info2("Delivering {} pending messages to body teller={}", pq.length : Integer, btlr)
				pq.toList.map(msg => {
					btlr.tellStrongCPMsg(msg)
				})
			})
		} else {
			myFailedBodyIDs.add(bodyID)
			val removedPQ_opt : Option[ListBuffer[VWBodyRq]] = myPendingBodyQueues.remove(bodyID)
			val len_opt : Option[Integer] = removedPQ_opt.map(_.length)
			warn2("Received empty body notice for bodyID={}, discarding {} pending TA-RQs, " +
						" and adding body to permanently failed list", bodyID, len_opt)

		}
	}
	private def requestBodyNotice(bodyID : Ident, whoDat : ActorRef) : Unit = {
		val answrTeller = new ActorRefCPMsgTeller[VWBodyNotice](whoDat)
		val findBody = new VWBodyFindRq(bodyID, answrTeller)
		getCharAdminTeller.tellStrongCPMsg(findBody)
	}
	def routeBodyRq(bodyID : Ident, msgForBody : VWBodyRq, whoDat : ActorRef) : Unit = {
		// Resolve message body-URI to bodyActor
		if (myBodyTlrsByID.isDefinedAt(bodyID)) {
			val bodyTlr = myBodyTlrsByID.get(bodyID).get
			bodyTlr.tellStrongCPMsg(msgForBody)
		} else if (myFailedBodyIDs.contains(bodyID)) {
			warn1("Ignoring request sent to known-failed body with ID={}", bodyID)
		} else {
			val pendingQueue : ListBuffer[VWBodyRq] = myPendingBodyQueues.getOrElseUpdate(bodyID, {
				info2("Requesting body notice for bodyID={} be delivered to {}", bodyID, whoDat)
				requestBodyNotice(bodyID, whoDat)
				new ListBuffer[VWBodyRq]
			})
			pendingQueue.append(msgForBody)
			info2("Appended body request to pending queue for bodyID={}, which now has len={}", bodyID, pendingQueue.length : Integer)
		}
	}
}
trait VWBodyTARouterLogic extends TARqExtractorHelp with MakesTransform3D  with VarargsLogging {
	protected def getMedialRendezvous :  VWBodyMedialRendezvous

	def handleBodyTA(ta : ThingActionSpec, gax: GoodyActionExtractor, whoDat : ActorRef) : Unit = {
		val bodyID = gax.getGoodyID
		val tvm = ta.getParamTVM
		val maybeXform : MaybeTransform3D = extractXform(tvm, gax)
		
		val opKind = gax.getKind
		opKind match {
			case GoodyActionExtractor.Kind.MOVE => {
                val dur_opt : Option[JFloat] = extractDuration(tvm)
				sendMedialVWBodyMoveRq(bodyID, maybeXform, dur_opt, whoDat)
			}
			case GoodyActionExtractor.Kind.CREATE => {
				error1("Cannot create VW-bodies using a TA-Rq.  Are you trying to find a body to command? Failed rq-TA={}", ta)
			}
			case GoodyActionExtractor.Kind.SET => {
				sendMedialVWBodyMoveRq(bodyID, maybeXform, Option.empty[JFloat], whoDat)
			}
			case GoodyActionExtractor.Kind.DELETE => {
                error1("Cannot delete VW-bodies using a TA-Rq.  Are you trying to find a body to command? Failed rq-TA={}", ta)
			}

		}

	}
	private def sendMedialVWBodyMoveRq(bodyID : Ident, maybeXform : MaybeTransform3D,
									   dur_opt : Option[JFloat], whoDat : ActorRef): Unit = {
		val concXform : Transform3D = makeDefiniteXForm(maybeXform)
		val manipGuts : ManipDesc = if (dur_opt.isDefined) {
			new SmooveManipEndingImpl(concXform, dur_opt.get)
		} else {
			new AbruptManipAbsImpl(concXform)
		}
		val msgForBody : VWBodyRq = new VWBodyManipRq(manipGuts)
		getMedialRendezvous.routeBodyRq(bodyID, msgForBody, whoDat)

	}
	def receiveBodyNotice(vwbn : VWBodyNotice) : Unit = {
		getMedialRendezvous.noticeBody(vwbn)
	}

}
trait CamTARouterLogic extends TARqExtractorHelp with MakesTransform3D with OuterCamHelp {
	protected def getVWPubTellers : VWorldPublicTellers

	def handleCameraTA(ta : ThingActionSpec, gax: GoodyActionExtractor, whoDat : ActorRef) : Unit = {
		// Resolve message cam-URI to paired shape ID, which is used for most camera movement control.
		// However, if we want to use ".lookAt" ...
		val camGuideShapeID = gax.getGoodyID
		val tvm = ta.getParamTVM

		val stageTeller : CPMsgTeller = getVWPubTellers.getStageTeller.get
		val spcTeller : CPMsgTeller = getVWPubTellers.getShaperTeller.get
		val opKind = gax.getKind
		opKind match {
			case GoodyActionExtractor.Kind.CREATE => {
				val camShortLabel: String = "camMadeByTARouter"
				val initCamPos : Vector3f = Vector3f.ZERO
				val initPointDir : Vector3f = Vector3f.UNIT_Z // .negate()
				val initCamState: CamState3D = new CamStateParams3D(initCamPos, initPointDir)
				val (left, right, bot, top) = (0.7f, 0.95f, 0.75f, 0.95f)
				val bgColor_opt = extractColor(gax) // .orElse(Some(ColorRGBA.Blue))
				val initVP = new ViewportDesc(left, right, bot, top, bgColor_opt)
				val camID : Ident = makeStampyRandyIdent(camShortLabel + "_intrnCam")
				makeAndBindExtraCam(stageTeller, spcTeller, camID, camGuideShapeID, initCamState, initVP)
			}
			case GoodyActionExtractor.Kind.MOVE => {
				info1("Processing cam-move request: {}", ta)

				val maybeXform : MaybeTransform3D = extractXform(tvm, gax)
				val dur_opt : Option[JFloat] = extractDuration(tvm)

				sentCamMoveRq(spcTeller, camGuideShapeID, maybeXform, dur_opt)
				/*
				val guideTgtPos = new Vector3f(-1.0f, 5.0f, 3.0f)
				val rotAngles = Array(45.0f, -45.0f, 15.0f)
				val guideTgtRot = new Quaternion(rotAngles)
				val guideTgtScale = Vector3f.UNIT_XYZ
				val guideTgtXform = new TransformParams3D(guideTgtPos, guideTgtRot, guideTgtScale)
				*/


				/*
					val camShortLabel: String = "camMadeByTARouter"
					val initCamState: CamState3D = null
					val initVP: ViewportDesc = null
					val camGuideID = makeAndBindExtraCam(stageTeller, spcTeller, camShortLabel, initCamState, initVP)
				*/
			}
			case GoodyActionExtractor.Kind.SET => {
                info1("Processing cam-set request: {}", ta)
				val maybeXform : MaybeTransform3D = extractXform(tvm, gax)
				sentCamMoveRq(spcTeller, camGuideShapeID, maybeXform, Option.empty[JFloat])
			}
			case GoodyActionExtractor.Kind.DELETE => {

			}
		}
	}
}
trait VWThingActReqRouterLogic extends VWBodyTARouterLogic with CamTARouterLogic with MakesTransform3D {


	lazy private val myBodyMedialRndzvs = new VWBodyMedialRendezvous {
		override def getCharAdminTeller: CPStrongTeller[VWBodyLifeRq] = getVWPubTellers.getCharAdminTeller.get
	}
	override protected def getMedialRendezvous :  VWBodyMedialRendezvous  = myBodyMedialRndzvs
	def routeRq(vwtarq: VWRqTAWrapper, whoDat : ActorRef) : Unit = {
		val tellers = getVWPubTellers
		info1("Router handling TARq in binary format: {}, and wisely using vwpt-tellers...", vwtarq)

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
			// VWGoodyJobLogic (in VWGoody.scala), which in turn invokes the old cogchar method:
			//   o.c.render.goody.basic.BasicGoodyCtxImpl.consumeAction(actSpec)
			// That object connection was attached during startup in
			// VWorldBossLogic.completeBossSetupAndPublish (in VWBoss.scala).

			val directLegacyGoodyTeller = getVWPubTellers.getGoodyDirectTeller
			directLegacyGoodyTeller.get.tellStrongCPMsg(vwtarq)
		}
	}

	// override def getCharAdminTeller : CPStrongTeller[VWBodyLifeRq] = getVWPubTellers.getCharAdminTeller.get

}
class VWThingActReqRouterActor(routerLogic : VWThingActReqRouterLogic) extends FrienduActor {
	override def receive = {
		case vwgrq: VWRqTAWrapper => {
			routerLogic.routeRq(vwgrq, self)
		}
		case vwbn: VWBodyNotice => {
			routerLogic.receiveBodyNotice(vwbn)
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