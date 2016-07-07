package org.friendularity.vwmsg

import org.friendularity.cpump.{CPMsgTeller, CPumpMsg}
import org.friendularity.vwmsg


/**
  * Created by StuB22 on 6/21/2016.
  *
  * For keyboard events and the like.
  */


trait VWUserInputNotice extends VWorldNotice

// External callbacks should only send externaizable (VM-independent, String-encodable) messages at
case class VWKeymapBinding_Extern(inpNamesToActionNames : Map[Char,String]) extends VWStageRqMsg

// Medial = parallel an implemented in "OuterLogic".  Callbacks may send messages to internal,
// which may be nonserial in many cases.

case class VWKeymapBinding_Medial(inpNamesToActionFuncs : Map[String,Function1[VWorldPublicTellers,Unit]],
								  pubTellers : VWorldPublicTellers) extends VWStageRqMsg

// We do not allow for a KeyBinding direct to Internal.  Stated another way:
// We do not allow for direct callback from keyboard into code that talks to the RenderRegistryClient, JME3, etc.
// That all must be done by actor messages triggered from the medial (or external) callback actions, responding to
// the messages above.
trait NavCmd extends CPumpMsg {

	def makeSendingFunc : Function1[VWorldPublicTellers,Unit] = pt => {
		val standinNavTeller : CPMsgTeller = pt.getStageTeller.get
		standinNavTeller.tellCPMsg(this)
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


