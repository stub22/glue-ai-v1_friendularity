package org.friendularity.vw.msg.intrn

import org.appdapter.core.name.Ident
import org.cogchar.api.humanoid.HumanoidFigureConfig
import org.cogchar.bind.mio.robot.svc.ModelBlendingRobotServiceContext
import org.friendularity.infra.cpmsg.CPStrongTeller
import org.friendularity.vw.msg.bdy.{VWBodyLifeRq, VWBodyNotice}

/**
  * Created by Owner on 2/18/2017.
  */
// Note this message is not serializable WHEN it contains a nonempty myMBRoboSvcCtx_opt, which represents
// all connection to the MechIO animation system.
// TODO:  BodyMaker should eventually be smart enough to find the OSGi and/or Qpid animation services it needs.
case class VWBodyMakeRq(dualBodyID: Ident, fullHumaCfg : HumanoidFigureConfig,
						myMBRoboSvcCtx_opt : Option[ModelBlendingRobotServiceContext],
						answerTeller : CPStrongTeller[VWBodyNotice]) extends VWBodyLifeRq
