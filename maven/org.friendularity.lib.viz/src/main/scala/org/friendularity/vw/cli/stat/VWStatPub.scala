package org.friendularity.vw.cli.stat

import akka.actor.Actor
import org.appdapter.fancy.log.VarargsLogging
import org.friendularity.infra.field.{MsgToStatusSrc, StatusTickMsg}

import org.friendularity.vw.impl.ta.OffersVWorldQpidServer
import org.friendularity.vw.msg.pub.VWorldPublicTellers

/**
  * Created by Owner on 8/8/2016.
  */

trait VWStatPubLogic extends VarargsLogging {
	protected def getPubTellers : VWorldPublicTellers
	protected def getQpidSvcOffering_opt : Option[OffersVWorldQpidServer] = None

	def gatherStatusAndSendHelpfulNotices : Unit = {
		val pubTellers = getPubTellers
		debug1("Time to gather some awesome status from pubTellers={}, and publish it out for exo-client use", pubTellers)
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
		case otherMsg : MsgToStatusSrc => statPubLogic.handleOtherStatusCtrlMsg(otherMsg)
	}
}