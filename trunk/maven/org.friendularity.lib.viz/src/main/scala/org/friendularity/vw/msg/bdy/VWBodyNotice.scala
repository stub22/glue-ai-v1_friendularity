package org.friendularity.vw.msg.bdy

import org.appdapter.core.name.Ident
import org.friendularity.cpmsg.CPStrongTeller
import org.friendularity.vw.msg.cor.VWorldNotice

/**
  * Code moved here on 1/17/2017.
  */
trait VWBodyNotice extends VWorldNotice {
	def getVWBodyID : Ident
	def getBodyTeller_opt : Option[CPStrongTeller[VWBodyRq]]
	// TODO:  Add getBodyState_opt which returns Option[RdfableState]
}

case class VWBodyNoticeImpl(bodyID : Ident, bodyTeller : Option[CPStrongTeller[VWBodyRq]]) extends VWBodyNotice {
	override def getVWBodyID: Ident = bodyID

	override def getBodyTeller_opt: Option[CPStrongTeller[VWBodyRq]] = bodyTeller
}

trait VWExoBodyChance extends VWorldNotice
