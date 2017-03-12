package org.friendularity.vw.msg.shp.deep

import org.appdapter.core.name.Ident
import org.friendularity.infra.cpmsg.CPStrongTeller
import org.friendularity.vw.mprt.manip.{Transform3D, ManipStatusPropagator, ManipStatusMsg, ManipCompletionHandle, ManipDesc}
import org.friendularity.vw.msg.cor.VWContentRq

/**
  * Created by Owner on 1/23/2017.
  */
trait VWShapeManipRq extends VWContentRq {
	def getTgtShapeID : Ident
	def getManipDesc : ManipDesc
	def getStatusHandler_opt : Option[ManipCompletionHandle] = None
}
case class ShapeManipRqImpl(myTgtShapeID : Ident, myManipDesc : ManipDesc,
							statusTlr_opt : Option[CPStrongTeller[ManipStatusMsg]]) extends VWShapeManipRq {
	override def getTgtShapeID: Ident = myTgtShapeID

	override def getManipDesc: ManipDesc = myManipDesc

	lazy private val statusHandlerOpt = statusTlr_opt.map(tlr => new ManipStatusPropagator(Option(tlr)))
	override def getStatusHandler_opt : Option[ManipCompletionHandle] = statusHandlerOpt
}

// Actual msgs we expect, with case-class impls below.
trait DoTransformAbsoluteNow extends VWShapeManipRq {
	// Abruptly moves the target to this new state.
	// "Abs" pos is w.r.t. parent, not world.
	def getAbsXform : Transform3D
}
trait DoTransformIncrementNow extends VWShapeManipRq {
	// Incremental change in each xform, based on current state of the target.
	// Use cases are mainly for casual scripting and experiments.
	// Is still a jump-state, presumably just a relatively small one.
	// Not intended for smooth animation - see Smoove* classes for that feature.
	// (Or for cine-style waypoint anim, see the VWCinePath)
	def getIncrementXform : Transform3D
}
