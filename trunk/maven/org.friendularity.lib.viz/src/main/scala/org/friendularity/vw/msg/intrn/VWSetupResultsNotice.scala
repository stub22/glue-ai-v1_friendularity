package org.friendularity.vw.msg.intrn

import org.cogchar.bind.midi.in.TempMidiBridge
import org.friendularity.vw.impl.sys.UpdateAttacher
import org.friendularity.vw.mprt.ingred.{BodyMgrIngred, LesserIngred}

/**
  * Created by Owner on 2/18/2017.
  */
// Boss eventually sends this response to the answerTeller of each VWARM_FindPublicTellers rcvd.
case class VWSetupResultsNotice(lesserIngred: LesserIngred,
								bodyMgrIngred: BodyMgrIngred,
								updAtchr : UpdateAttacher, tmb_opt : Option[TempMidiBridge]) extends VWorldInternalNotice
