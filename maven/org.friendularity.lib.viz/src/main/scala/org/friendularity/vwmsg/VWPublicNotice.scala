package org.friendularity.vwmsg

import org.appdapter.core.name.Ident
import org.friendularity.cpmsg.CPStrongTeller

/*
// VWorldPublicTellers = client access to main features for application building.
// A vworldBoss supplies this serializable directory of its actors to any client who asks.
// From here clients can navigate to all published vworld service actors.
// But a client may also know+find these same actors and other actors by other means.
// This trait is provided as a good starting point, not as the definitive or exhaustive API.
*/

trait VWorldPublicTellers extends VWorldNotice  {
	// Used to create and animate arbitrary shapes and nodes.
	def getShaperTeller : Option[CPStrongTeller[VWShapeCreateRq]] = None

	// Current impl for getGoodyDirectTeller still uses old Cogchar's BasicGoodyCtxImpl.
	// That shape mapping should eventually be replacable with what Shaper actor above does.
	// In that case, and if RDF-to-binTA translation always happens upstream (which is current design
	// as of 2016-08-12) THEN there will be no need for this GoodyTeller Actor.
	def getGoodyDirectTeller : Option[CPStrongTeller[VWRqTAWrapper]] = None

	// Used to create, find, and broadcast to VWBodies, which each has a separate actor.
	def getCharAdminTeller : Option[CPStrongTeller[VWBodyLifeRq]] = None

	// Stage = Catch-all used to do other JME setup, including cameras, lights, keyboard and mouse, sky, ...
	// We are always seeking to simplify this part, while getting best possible OpenGL output + response.
	// TODO:  Cameras should probably get their own actors, similar to bodies.
	def getStageTeller : Option[CPStrongTeller[VWStageRqMsg]] = None

	// Used to display+hide+modify 2D screens of info and GUI controls.  (Press "U" to show/hide)
	def getOverlayTeller : Option[CPStrongTeller[VWOverlayRq]] = None
}
case class VWPubTellersMsgImpl(goodyTeller : CPStrongTeller[VWRqTAWrapper],
							   charAdminTeller: CPStrongTeller[VWBodyLifeRq],
							   shaperTeller : CPStrongTeller[VWShapeCreateRq],
							   stageTeller: CPStrongTeller[VWStageRqMsg],
							   ovlTeller : CPStrongTeller[VWOverlayRq]) extends VWorldPublicTellers {
	override def getGoodyDirectTeller : Option[CPStrongTeller[VWRqTAWrapper]] = Option(goodyTeller)
	override def getCharAdminTeller : Option[CPStrongTeller[VWBodyLifeRq]] = Option(charAdminTeller)
	override def getShaperTeller : Option[CPStrongTeller[VWShapeCreateRq]] = Option(shaperTeller)
	override def getStageTeller : Option[CPStrongTeller[VWStageRqMsg]] = Option(stageTeller)
	override def getOverlayTeller : Option[CPStrongTeller[VWOverlayRq]] = Option(ovlTeller)
}

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
