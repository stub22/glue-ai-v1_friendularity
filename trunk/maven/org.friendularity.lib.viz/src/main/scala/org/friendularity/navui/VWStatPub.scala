package org.friendularity.navui

import akka.actor.Actor
import org.appdapter.fancy.log.VarargsLogging
import org.friendularity.field.{StatusTickMsg, MsgToStatusSrc}
import org.friendularity.qpc.OffersQpidSvcs
import org.friendularity.vwmsg.VWorldPublicTellers

/**
  * Created by Owner on 8/8/2016.
  */

trait VWStatPubLogic extends VarargsLogging {
	protected def getPubTellers : VWorldPublicTellers
	protected def getQpidSvcOffering_opt : Option[OffersQpidSvcs] = None

	def gatherStatusAndSendHelpfulNotices : Unit = {
		info0("Time to gather some awesome status and publish it out for exo-client use")
		val qpidOffering_opt = getQpidSvcOffering_opt
		qpidOffering_opt.map(qpo => {
			val vwPubNoticeSender = qpo.getVWPubNoticeSender
			vwPubNoticeSender.sendPingNotice("Helpful stat notice from VWStatPubLogic")
		})
	}
	def handleOtherStatusCtrlMsg(msgToStatSrc : MsgToStatusSrc) : Unit = {
		warn1("Ignoring status ctrl msg: {}", msgToStatSrc)
	}
}
class VWStatPubActor(statPubLogic : VWStatPubLogic)  extends Actor {
	def receive = {
		case stm: StatusTickMsg => statPubLogic.gatherStatusAndSendHelpfulNotices
		case otherMsg : MsgToStatusSrc =>
	}
}