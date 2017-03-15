package org.friendularity.navui

/**
  * Created by Owner on 4/1/2016.
  */

import org.cogchar.api.owrap.{scrn => cscrn}
import cscrn._
import org.friendularity.infra.cpmsg.{CPMsgTeller, CPumpMsg}

class WinBind(createOp : OSAWindowCreate, inTeller : CPMsgTeller) {
	def processScrnOp(outScrnAct : OSAScreenOp): Unit = {

	}
	def processUserAct(item : DisplayItemDesc, inDat : AnyRef) {
		val inMsg : CPumpMsg = ???
		inTeller.tellCPMsg(inMsg)
	}
	def findItemForGoody() : Option[DisplayItemDesc] = {
		None
	}
}
