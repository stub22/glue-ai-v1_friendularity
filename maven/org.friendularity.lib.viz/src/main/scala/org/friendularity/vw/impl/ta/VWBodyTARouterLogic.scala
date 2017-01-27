package org.friendularity.vw.impl.ta

import akka.actor.ActorRef
import org.appdapter.core.name.Ident
import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.api.thing.ThingActionSpec
import org.cogchar.render.app.entity.GoodyActionExtractor
import org.friendularity.vw.mprt.manip.{MaybeTransform3D, MakesManipDesc}
import org.friendularity.vw.msg.bdy.{VWBodyNotice, VWBodyManipRq, VWBodyRq}

import java.lang.{Float => JFloat, Integer => JInt, Long => JLong}
/**
  * Created by Owner on 1/26/2017.
  */
trait VWBodyTARouterLogic extends TARqExtractorHelp with MakesManipDesc  with VarargsLogging {
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
				sendMedialVWBodyMoveRq(bodyID, maybeXform, None, whoDat)
			}
			case GoodyActionExtractor.Kind.DELETE => {
				error1("Cannot delete VW-bodies using a TA-Rq.  Are you trying to find a body to command? Failed rq-TA={}", ta)
			}

		}

	}
	private def sendMedialVWBodyMoveRq(bodyID : Ident, maybeXform : MaybeTransform3D,
									   dur_opt : Option[JFloat], whoDat : ActorRef): Unit = {
		val forceToFullXform = false // "Partial" approach is preferred as of 2016-Nov, see RVWS-49 and RVWS-57.
		val manipGuts = makeManipGuts(maybeXform, dur_opt, forceToFullXform)

		val msgForBody : VWBodyRq = new VWBodyManipRq(manipGuts)
		getMedialRendezvous.routeBodyRq(bodyID, msgForBody, whoDat)
	}
	def receiveBodyNotice(vwbn : VWBodyNotice) : Unit = {
		getMedialRendezvous.noticeBody(vwbn)
	}
}