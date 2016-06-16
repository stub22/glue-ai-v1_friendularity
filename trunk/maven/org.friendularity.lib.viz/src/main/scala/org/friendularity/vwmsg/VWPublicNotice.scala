package org.friendularity.vwmsg

import org.friendularity.cpump.CPStrongTeller


// The vworldBoss supplies this serializable directory of its actors to any client who asks.
// From here clients can navigate to all published vworld service actors.
// Client may also know+find these same actors and other actors by other means.
// This trait is made available as a helpful starting point, not as the definitive or exhaustive API.
trait VWorldPublicTellers extends VWorldNotice  {

	def getGoodyDirectTeller : Option[CPStrongTeller[VWGoodyRqActionSpec]] = None
	def getCharAdminTeller : Option[CPStrongTeller[VWBodyLifeRq]] = None
}
case class VWPubTellersMsgImpl(goodyTeller : CPStrongTeller[VWGoodyRqActionSpec],
							   charAdminTeller: CPStrongTeller[VWBodyLifeRq] ) extends VWorldPublicTellers {
	override def getGoodyDirectTeller : Option[CPStrongTeller[VWGoodyRqActionSpec]] = Option(goodyTeller)
	override def getCharAdminTeller : Option[CPStrongTeller[VWBodyLifeRq]] = Option(charAdminTeller)
}

trait VWBodyNotice extends VWorldNotice {
	def getBodyTeller : CPStrongTeller[VWBodyRq]
}
