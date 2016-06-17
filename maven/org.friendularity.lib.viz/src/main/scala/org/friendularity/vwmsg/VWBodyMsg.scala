package org.friendularity.vwmsg

import org.appdapter.core.name.Ident
import org.cogchar.api.humanoid.HumanoidFigureConfig
import org.cogchar.bind.mio.robot.svc.ModelBlendingRobotServiceContext
import org.friendularity.cpump.CPStrongTeller


trait VWBodyLifeRq extends VWorldRequest // disjoint from VWBodyRq

case class VWBodyMakeRq(dualBodyID: Ident, fullHumaCfg : HumanoidFigureConfig,
						myMBRoboSvcCtx_opt : Option[ModelBlendingRobotServiceContext],
						answerTeller : CPStrongTeller[VWBodyNotice]) extends VWBodyLifeRq

// Message sent directly to a particular existing VWBody, not for creation/deletion of same.
trait VWBodyRq extends VWorldRequest // disjoint from VWBodyLifeRq

case class VWBodyMoveRq(xPos : Float, yPos : Float, zPos : Float) extends VWBodyRq

