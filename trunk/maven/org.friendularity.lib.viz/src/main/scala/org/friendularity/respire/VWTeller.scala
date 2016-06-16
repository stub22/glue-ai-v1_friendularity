package org.friendularity.respire

import org.friendularity.cpump.{CPumpMsg, CPStrongTeller, CPMsgTeller}

/**
  * Created by Owner on 6/6/2016.
  */


// The vworldBoss supplies this serializable directory of its actors to any client who asks.
// From here clients can navigate to all published vworld service actors.
// Client may also know+find these same actors and other actors by other means.
// This trait is made available as a helpful starting point, not as the definitive or exhaustive API.
trait VWorldPublicTellers extends VWorldNotice  {

	def getGoodyDirectTeller : Option[CPStrongTeller[VWGoodyRqActionSpec]] = None
	def getCharAdminTeller : Option[CPStrongTeller[VWCharAdminRq]] = None
}
case class VWPubTellersImpl(goodyTeller : CPStrongTeller[VWGoodyRqActionSpec],
				charAdminTeller: CPStrongTeller[VWCharAdminRq] ) extends VWorldPublicTellers {
	override def getGoodyDirectTeller : Option[CPStrongTeller[VWGoodyRqActionSpec]] = Option(goodyTeller)
	override def getCharAdminTeller : Option[CPStrongTeller[VWCharAdminRq]] = Option(charAdminTeller)
}

trait VWBodyNotice extends VWorldNotice {
	def getBodyTeller : CPStrongTeller[VWBodyRq]
}
