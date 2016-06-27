package org.friendularity.vwmsg

import org.appdapter.core.name.Ident
import org.cogchar.api.humanoid.HumanoidFigureConfig
import org.friendularity.cpump.CPStrongTeller
import org.cogchar.bind.mio.robot.svc.ModelBlendingRobotServiceContext // Nonserializable - See further notes below.

trait VWBodyLifeRq extends VWorldRequest // disjoint from VWBodyRq


// Note this message is not serializable WHEN it contains a nonempty myMBRoboSvcCtx_opt, which represents
// all connection to the MechIO animation system.
// TODO:  BodyMaker should eventually be smart enough to find the OSGi and/or Qpid animation services it needs.
case class VWBodyMakeRq(dualBodyID: Ident, fullHumaCfg : HumanoidFigureConfig,
						myMBRoboSvcCtx_opt : Option[ModelBlendingRobotServiceContext],
						answerTeller : CPStrongTeller[VWBodyNotice]) extends VWBodyLifeRq

case class VWBroadcastToAllBodies(bodyRQ : VWBodyRq) extends VWBodyLifeRq


// Message sent directly to a particular existing VWBody, not for creation/deletion of same.
trait VWBodyRq extends VWorldRequest // disjoint from VWBodyLifeRq

case class VWBodyMoveRq(xPos : Float, yPos : Float, zPos : Float) extends VWBodyRq

case class VWBodySkeletonDisplayToggle()  extends VWBodyRq

