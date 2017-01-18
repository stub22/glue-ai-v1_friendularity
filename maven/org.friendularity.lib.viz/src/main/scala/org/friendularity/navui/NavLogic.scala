/*
 *  Copyright 2016 by The Friendularity Project (www.friendularity.org).
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
package org.friendularity.navui

import org.friendularity.vw.msg.stg.{InnerNavCmds, NavCmdImpl, NavCmdKeyClkBind, NavCmd}
import org.friendularity.vwmsg.VWorldPublicTellers

/** Created by Stub22 on 7/7/2016.
  */


/*
trait NavLogic // Placeholder matching our .scala filename
case class NavPageImpl()
trait NavCursor
*/

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
		"B" -> NCmd_PAG_PREV,
		"M" -> NCmd_PAG_NEXT
	)

	val navCmdKeyBindMap: Map[String, Function1[VWorldPublicTellers, Unit]] =
		navCmdBindPairs.toMap.mapValues(_.makeSendingFunc)

}


