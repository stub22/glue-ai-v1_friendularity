package org.friendularity.vw.msg.cor

import org.friendularity.cpmsg.CPumpMsg
import org.friendularity.vw.impl.ovl.OverlayPage

// Our most general vw-msg traits are all simply type markers, adding no new methods.
trait VWorldMsg extends CPumpMsg

// Rq => Inbound to some system (e.g. internal = VWorld core)
trait VWorldRequest  extends VWorldMsg


trait VWContentRq extends VWorldRequest

/* These make sense as design abstrcts, equivalent to constructs:  A, B,
but were never actively used.

trait VWCoreRq extends VWorldRequest {
	// Used to talk to internal "VWCore" actor
}
trait VWSceneCoreRq extends VWCoreRq {
	// Describes a change to managed VW scene graph, to be translated (usually by VWCore actor)
	// into calls on JME render thread.
}
*/

