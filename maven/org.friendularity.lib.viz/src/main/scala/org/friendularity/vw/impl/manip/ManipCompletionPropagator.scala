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

import com.jme3.animation.{AnimChannel, AnimControl, AnimEventListener}
import org.appdapter.fancy.log.VarargsLogging
import org.friendularity.vw.mprt.manip.ManipCompletionHandle

import com.jme3.animation.{AnimChannel, AnimControl => JmeAnimCtrl, AnimEventListener => JmeAnimEventListener, Animation => JmeGrossAnim}

/**
  * Created by Owner on 1/19/2017.
  */
class ManipCompletionPropagator(cmplHndl : ManipCompletionHandle) extends JmeAnimEventListener with VarargsLogging {
	// Does this get called when an animation is interrupted/cancelled ?
	override def onAnimCycleDone(animControl: JmeAnimCtrl, animChannel: AnimChannel, animName: String): Unit = {
		// Causes co-modification exceptions
		// animControl.removeListener(this)
		info2("Propagating onAnimCycleDone for animName={} to cmplHndl={}", animName, cmplHndl)
		cmplHndl.notifyComplete(animName, "anim=[" + animName  + "] chan=[" + animChannel + "]")
	}

	override def onAnimChange(animControl: JmeAnimCtrl, animChannel: AnimChannel, animName: String): Unit = {
		warn2("Ignoring onAnimChange for animName={} to cmplHndl={}", animName, cmplHndl)
	}
}
