package org.friendularity.vw.msg.intrn

import org.friendularity.navui.NavAppCloser
import org.friendularity.vw.msg.cor.VWorldRequest

/**
  * Created by Owner on 2/18/2017.
  */
// TODO:  This nonserializable NavAppCloser reference should be replaced by separate message pathway.
case class VWSetupRq_Lnch(wrapInSwingCanv : Boolean, fixmeClzrNonSerial : NavAppCloser) extends VWorldRequest {
	// Sent from NuiiApp to VWBoss, as of 2016-06-16
}
