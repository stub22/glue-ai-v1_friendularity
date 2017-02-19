package org.friendularity.vw.msg.nav

import org.friendularity.cpmsg.CPMsgTeller
import org.friendularity.vw.msg.cor.VWOverlayRq

import org.friendularity.vw.msg.pub.VWorldPublicTellers

/**
  * Created by Owner on 2/18/2017.
  */
trait NavCmds

// We do not allow for a KeyBinding direct to Internal.  Stated another way:
// We do not allow for direct callback from keyboard into code that talks to the RenderRegistryClient, JME3, etc.
// That all must be done by actor messages triggered from the medial (or external) callback actions, responding to
// the messages above.
trait NavCmd extends VWOverlayRq {

	def makeSendingFunc : Function1[VWorldPublicTellers,Unit] = pt => {
		val standinNavTeller : CPMsgTeller = pt.getStageTeller.get
		val ovlTeller = pt.getOverlayTeller.get
		ovlTeller.tellCPMsg(this)
	}
}
trait NavCmdKeyClkBind

// Case class => Two cmds constructed with same name are .equal() and should have same hashCode.
case class NavCmdImpl(cmdName : String) extends NavCmd

trait InnerNavCmds {
	val NCmd_SHOW_TOGGLE : NavCmd = NavCmdImpl("navShowToggle")
	val NCmd_GO_IN : NavCmd = NavCmdImpl("navGoIn")
	val NCmd_GO_OUT : NavCmd = NavCmdImpl("navGoOut")
	val NCmd_GO_LEFT : NavCmd = NavCmdImpl("navGoLeft")
	val NCmd_GO_RIGHT : NavCmd = NavCmdImpl("navGoRight")
	val NCmd_GO_UP : NavCmd = NavCmdImpl("navGoUp")
	val NCmd_GO_DOWN : NavCmd = NavCmdImpl("navGoDown")
	val NCmd_PAG_PREV : NavCmd = NavCmdImpl("navPagPrv")
	val NCmd_PAG_NEXT : NavCmd = NavCmdImpl("navPagNxt")
}

