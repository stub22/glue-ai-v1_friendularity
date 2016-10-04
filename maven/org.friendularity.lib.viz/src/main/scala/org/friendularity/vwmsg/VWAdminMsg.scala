package org.friendularity.vwmsg

import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.bind.midi.in.TempMidiBridge
import org.friendularity.cpmsg.{CPStrongTeller, CPMsgTeller}

import org.friendularity.vwimpl.{UpdateAttacher}


trait VWAdminRqMsg extends VWorldRequest with VarargsLogging

case class VWARM_GreetFromPumpAdmin(pumpAdminTeller : CPMsgTeller) extends VWAdminRqMsg
// case class VWARM_FindGoodyTeller(answerTeller: CPMsgTeller) extends VWAdminRqMsg

// Receiver can wait to answer until the system is sufficiently ready, e.g. until the VWorld is up.
// However, Sndr may inquire well after the VWorld is up, and then Rcvr should answer right away.
case class VWARM_FindPublicTellers(answerTeller: CPStrongTeller[VWorldPublicTellers]) extends VWAdminRqMsg


case class VWSetupRq_Conf() extends VWorldRequest // Not being sent as of 2016-06-16

case class VWSetupRq_Lnch(wrapInSwingCanv : Boolean) extends VWorldRequest {
	// Sent from NuiiApp to VWBoss, as of 2016-06-16
}

// Boss eventually sends this response to the answerTeller of each VWARM_FindPublicTellers rcvd.
case class VWSetupResultsNotice(lesserIngred: LesserIngred,
								bodyMgrIngred: BodyMgrIngred,
								updAtchr : UpdateAttacher, tmb_opt : Option[TempMidiBridge]) extends VWorldInternalNotice

