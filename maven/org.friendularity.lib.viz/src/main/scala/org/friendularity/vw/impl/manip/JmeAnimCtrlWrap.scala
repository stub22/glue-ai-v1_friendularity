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

import com.jme3.scene.Spatial
import org.appdapter.fancy.log.VarargsLogging
import com.jme3.animation.{AnimChannel, AnimControl => JmeAnimCtrl, AnimEventListener => JmeAnimEventListener, Animation => JmeGrossAnim, AnimationFactory, LoopMode}
import org.friendularity.vw.mprt.manip.ManipCompletionHandle

/**
  * Code moved to new file on 1/19/2017.
  */
trait JmeAnimCtrlWrap extends VarargsLogging {
	// JmeAnimCtrl = Trigger point + update bridge for a set of animations, over many channels,
	def findAnimCtrl(s : Spatial) : Option[JmeAnimCtrl] = {
		val c = s.getControl(classOf[JmeAnimCtrl])
		Option(c)
	}

	def findOrMakeAnimCtrl(s : Spatial) : JmeAnimCtrl = {
		val ctrl : JmeAnimCtrl = {
			val copt = findAnimCtrl(s)
			copt.getOrElse {
				val nc = new JmeAnimCtrl()
				s.addControl(nc)
				nc
			}
		}
		ctrl
	}
	def cancelOldAnims(ac : JmeAnimCtrl) : Unit = {
		ac.clearChannels()
	}
	/*
	def haltAndDetachAnimCtrl(ac : JmeAnimCtrl) : Unit = {
		cancelOldAnims(ac)  // This is necessary for the control "let go" of positions.
		// Curious if this part is really needed, since the anims are now cancelled.
		// ac.setEnabled(false)
	}
	*/
	def cancelAnyOldAnimsFound(s : Spatial) : Unit = {
		val ctrlOpt : Option[JmeAnimCtrl] = findAnimCtrl(s)
		ctrlOpt.map(ac => cancelOldAnims(ac))  // Note that this calls clearChannels
	}
	/*
	def ensureAnimCtrlEnabled(s : Spatial) : Unit = {
		val ac = findOrMakeAnimCtrl(s)
		ac.setEnabled(true)
	}
	*/
	def fireAnim(ac : JmeAnimCtrl, a: JmeGrossAnim, blendTimeSec : Float, loopMode : LoopMode) : Unit = {
		ac.addAnim(a) // Each anim may have multiple tracks of type:  SpatialTrack, BoneTrack, AudioTrack, EffectTrack
		val chan = ac.createChannel

		ac.setEnabled(true) // Will generally stay enabled until some direct abrupt move is sent
		chan.setAnim(a.getName, blendTimeSec) // This step "activates" the ctrl-chan-anim combo, yes?
		chan.setLoopMode(LoopMode.DontLoop)  // Cogchar lore says this should be done after setAnim
	}

	def fireAnimUnloopedUnblended(ac : JmeAnimCtrl, a: JmeGrossAnim, cmplHndl_opt : Option[ManipCompletionHandle]) : Unit = {
		debug1("********** Preparing to fire grossAnim={} - canceling old anims and clearing listeners, first", a)
		cancelOldAnims(ac)  // Note that this calls clearChannels
		ac.clearListeners()
		cmplHndl_opt.map(registerPropagator(ac, _))
		val blendTimeSec = 0f
		fireAnim(ac, a, blendTimeSec, LoopMode.DontLoop)
		trace1("Completed firing of grossAnim={}", a)
	}
	def fireAnimUnloopedUnblended(s : Spatial, a: JmeGrossAnim, cmplHndl_opt : Option[ManipCompletionHandle]) : Unit = {
		val ac = findOrMakeAnimCtrl(s)
		fireAnimUnloopedUnblended(ac, a, cmplHndl_opt)
	}

	def registerListener(ac : JmeAnimCtrl, ael : JmeAnimEventListener): Unit = {
		ac.addListener(ael)
	}

	def registerPropagator(ac : JmeAnimCtrl, cmplHndl : ManipCompletionHandle) : Unit = {
		val propagator = new JmeAnimStatusPropagator(cmplHndl)
		registerListener(ac, propagator)
	}
	def registerPropagator(s : Spatial, cmplHndl : ManipCompletionHandle) : Unit = {
		val ac = findOrMakeAnimCtrl(s)
		registerPropagator(ac, cmplHndl)
	}

}
