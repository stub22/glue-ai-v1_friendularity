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
import org.friendularity.vw.impl.tsk.FullJmeEnqHlp
import org.friendularity.vw.mprt.manip.{AbruptManipAbs, SmooveManipEnding, SmooveManipStory, ManipCompletionHandle, ManipDesc}

/**
  * Code moved to new file on 1/19/2017.
  */
trait Manipable extends Smoovable with IdentHlp with VarargsLogging {
	def applyManipDesc(manip : ManipDesc, enqHelp : FullJmeEnqHlp, ch_opt: Option[ManipCompletionHandle]) : Unit = {
		val ch : ManipCompletionHandle = ch_opt.getOrElse(new ManipCompletionHandle with VarargsLogging {
			override def notifyComplete(animName : String, dbg : Any): Unit = {
				debug3("Default completion for handleID={}, dbg={}, manip={}", myDummyHandleID, dbg.asInstanceOf[Object], manip)
			}
			private val myDummyHandleID = makeStampyRandyIdent("dummyCmpltnHndlr")

			override def getHandleID: Ident = myDummyHandleID
		})
		manip match {
			case smf : SmooveManipStory => {
				info1("Starting full-smoove manip: {}", smf)
				applySmooveNow_anyThrd(smf, ch)

			}
			case sme : SmooveManipEnding => {
				info1("Starting half-smoove manip: {}", sme)
				applySmooveFromCurrent_mystThrd(sme, ch)
			}
			case ama : AbruptManipAbs => {
				// val xformFull = ama.getXform_finish_full
				val xformPart = ama.getXform_finish_partial
				val func : Function0[Unit] = () => {
					applyTransform_partial_runThrd(xformPart)
					ch.notifyComplete("ABRUPT_NO_ANIM", "abrubtXform_partial=[" + xformPart + "]")
				}
				enqHelp.enqueueJmeCallable(func)

			}
		}
	}

}
