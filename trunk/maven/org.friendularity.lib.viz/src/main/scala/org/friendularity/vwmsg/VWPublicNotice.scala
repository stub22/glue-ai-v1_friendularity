package org.friendularity.vwmsg

import org.friendularity.cpmsg.CPStrongTeller


// The vworldBoss supplies this serializable directory of its actors to any client who asks.
// From here clients can navigate to all published vworld service actors.
// Client may also know+find these same actors and other actors by other means.
// This trait is made available as a helpful starting point, not as the definitive or exhaustive API.
trait VWorldPublicTellers extends VWorldNotice  {

	def getGoodyDirectTeller : Option[CPStrongTeller[VWGoodyRqTAWrapper]] = None
	def getCharAdminTeller : Option[CPStrongTeller[VWBodyLifeRq]] = None
	def getShaperTeller : Option[CPStrongTeller[VWShapeCreateRq]] = None
	def getStageTeller : Option[CPStrongTeller[VWStageRqMsg]] = None
	def getOverlayTeller : Option[CPStrongTeller[VWOverlayRq]] = None
}
case class VWPubTellersMsgImpl(goodyTeller : CPStrongTeller[VWGoodyRqTAWrapper],
							   charAdminTeller: CPStrongTeller[VWBodyLifeRq],
							   shaperTeller : CPStrongTeller[VWShapeCreateRq],
							   stageTeller: CPStrongTeller[VWStageRqMsg],
							   ovlTeller : CPStrongTeller[VWOverlayRq]) extends VWorldPublicTellers {
	override def getGoodyDirectTeller : Option[CPStrongTeller[VWGoodyRqTAWrapper]] = Option(goodyTeller)
	override def getCharAdminTeller : Option[CPStrongTeller[VWBodyLifeRq]] = Option(charAdminTeller)
	override def getShaperTeller : Option[CPStrongTeller[VWShapeCreateRq]] = Option(shaperTeller)
	override def getStageTeller : Option[CPStrongTeller[VWStageRqMsg]] = Option(stageTeller)
	override def getOverlayTeller : Option[CPStrongTeller[VWOverlayRq]] = Option(ovlTeller)
}

trait VWBodyNotice extends VWorldNotice {
	def getBodyTeller : CPStrongTeller[VWBodyRq]
}
trait VWExoBodyChance extends VWorldNotice
