package org.friendularity.vw.msg.bdy

import com.jme3.renderer.Camera
import org.appdapter.core.name.Ident
//import org.cogchar.api.humanoid.HumanoidFigureConfig
//import org.cogchar.bind.mio.robot.svc.ModelBlendingRobotServiceContext
import org.friendularity.infra.cpmsg.CPStrongTeller
import org.friendularity.vw.mprt.manip.ManipDesc
import org.friendularity.vw.msg.cor.VWorldRequest


trait VWBodyLifeRq extends VWorldRequest // disjoint from VWBodyRq

case class VWBodyFindRq(dualBodyID: Ident, answerTeller : CPStrongTeller[VWBodyNotice]) extends VWBodyLifeRq

case class VWBroadcastToAllBodies(bodyRQ : VWBodyRq) extends VWBodyLifeRq


// Message sent directly to a particular existing VWBody, not for creation/deletion of same.
trait VWBodyRq extends VWorldRequest // disjoint from VWBodyLifeRq

// Use VWBodyManipRq
// case class VWBodyMoveRq(xPos : Float, yPos : Float, zPos : Float) extends VWBodyRq

case class VWBodyManipRq(manipGuts : ManipDesc) extends VWBodyRq

case class VWBodySkeletonDisplayToggle()  extends VWBodyRq

case class VWBodyDangerYogaRq()  extends VWBodyRq

case class VWBodyAttachCamToBone(cam : Camera, boneName : String) extends VWBodyRq
