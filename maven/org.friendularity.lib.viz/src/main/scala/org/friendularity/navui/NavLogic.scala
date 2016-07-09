package org.friendularity.navui

import org.friendularity.vwmsg.{VWorldPublicTellers, NavCmdImpl, InnerNavCmds, NavCmdKeyClkBind, NavCmd}

/** Created by Stub22 on 7/7/2016.
  */


trait NavLogic // Placeholder matching our .scala filename

case class NavPageImpl()
trait NavCursor {

}

case class NavCmdKeyClkBindImpl(cmd : NavCmd, keyClickName : String) extends NavCmdKeyClkBind

trait OuterBindNavCmdKeys extends InnerNavCmds {
	// For normal use
	def makeNavCmdBind(cmd : NavCmd, keyClickName : String): NavCmdKeyClkBind = {
		NavCmdKeyClkBindImpl(cmd, keyClickName)
	}

	// For extensions
	def makeNavCmdBind(cmdName : String, keyClickName : String): NavCmdKeyClkBind = {
		// This makes new case-instances which may compare .equal() to compiled case instances above.
		makeNavCmdBind(NavCmdImpl(cmdName), keyClickName)
	}

	val navCmdBindPairs = List(
		"U" -> NCmd_SHOW_TOGGLE,
		"H" -> NCmd_GO_IN,
		"T" -> NCmd_GO_OUT,
		"Y" -> NCmd_GO_UP,
		"N" -> NCmd_GO_DOWN,
		"G" -> NCmd_GO_LEFT,
		"J" -> NCmd_GO_RIGHT,
		"B" -> NCmd_GO_RIGHT,
		"B" -> NCmd_PAG_PREV,
		"M" -> NCmd_PAG_NEXT
	)

	val navCmdKeyBindMap: Map[String, Function1[VWorldPublicTellers, Unit]] =
		navCmdBindPairs.toMap.mapValues(_.makeSendingFunc)

}


