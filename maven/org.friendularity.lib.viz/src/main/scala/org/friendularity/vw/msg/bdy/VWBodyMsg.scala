package org.friendularity.vw.msg.bdy

import com.jme3.renderer.Camera
import org.appdapter.core.name.Ident
import org.cogchar.api.humanoid.HumanoidFigureConfig
import org.cogchar.bind.mio.robot.svc.ModelBlendingRobotServiceContext
import org.friendularity.cpmsg.CPStrongTeller
import org.friendularity.vw.mprt.manip.ManipDesc
import org.friendularity.vw.msg.cor.VWorldRequest

// Nonserializable - See further notes below.

trait VWBodyLifeRq extends VWorldRequest // disjoint from VWBodyRq


// Note this message is not serializable WHEN it contains a nonempty myMBRoboSvcCtx_opt, which represents
// all connection to the MechIO animation system.
// TODO:  BodyMaker should eventually be smart enough to find the OSGi and/or Qpid animation services it needs.
case class VWBodyMakeRq(dualBodyID: Ident, fullHumaCfg : HumanoidFigureConfig,
						myMBRoboSvcCtx_opt : Option[ModelBlendingRobotServiceContext],
						answerTeller : CPStrongTeller[VWBodyNotice]) extends VWBodyLifeRq

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
