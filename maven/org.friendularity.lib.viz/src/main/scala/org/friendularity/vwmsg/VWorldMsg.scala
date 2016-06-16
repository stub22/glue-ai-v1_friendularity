package org.friendularity.vwmsg

import org.friendularity.cpump.CPumpMsg

// Our most general vw-msg traits are all simply type markers, adding no new methods.
trait VWorldMsg extends CPumpMsg

// Rq => Inbound to some system (e.g. internal = VWorld core)
trait VWorldRequest  extends VWorldMsg

// Notice => Outbound from something
trait VWorldNotice extends VWorldMsg

// Outbound from VWorld core
trait VWorldInternalNotice extends  VWorldNotice

trait VWContentRq extends VWorldRequest

trait VWCoreRq extends VWorldRequest {
	// Used to talk to internal "VWCore" actor
}
trait VWSceneCoreRq extends VWCoreRq {
	// Describes a change to managed VW scene graph, to be translated (usually by VWCore actor)
	// into calls on JME render thread.
}
