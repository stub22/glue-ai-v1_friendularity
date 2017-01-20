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

package org.friendularity.vw.impl.manip

import org.appdapter.core.name.Ident
import org.appdapter.fancy.log.VarargsLogging
import org.friendularity.util.IdentHlp
import org.friendularity.vw.mprt.manip.ManipCompletionHandle

/**
  * Created by Owner on 1/19/2017.
  */
class WrappedMCH(onlyAnimName : String, delegate : ManipCompletionHandle)
			extends ManipCompletionHandle with IdentHlp with VarargsLogging {
	def notifyComplete(animName : String, dbgBonus : Any) : Unit = {
		if (animName.equals(onlyAnimName)) {
			info3("WrappedMCH ID={} is ALLOWING propagation of completion for anim={} to delegate with ID={}",
				getHandleID, animName, delegate.getHandleID)
			delegate.notifyComplete(animName, dbgBonus)
		} else {
			warn3("WrappedMCH ID={} is DENYING propagation of completion for anim={}, which does not match our filter: {}",
				getHandleID, animName, onlyAnimName)
		}
	}
	lazy  private val myHandleID = makeStampyRandyIdent("wrappedMCH")
	def getHandleID : Ident = myHandleID
}
