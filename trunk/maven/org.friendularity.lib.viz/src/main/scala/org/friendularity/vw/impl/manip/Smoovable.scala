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
import com.jme3.animation.{Animation => JmeGrossAnim, AnimationFactory, LoopMode}

import org.appdapter.fancy.log.VarargsLogging
import org.friendularity.vw.mprt.manip.{SmooveManipStoryFullImpl, SmooveManipStoryPartialImpl, SmooveManipEnding, ManipCompletionHandle, SmooveManipStory, Transform3D, MaybeTransform3D}

/**
  * Code moved to new file on 1/19/2017.
  */
trait Smoovable extends AbruptlyMovable with Locatable with Addressable with VarargsLogging {
	val myAnimMaker = new JmeAnimMaker {}

	// applyTransform_runThrd cannot be used reliably on a spatial that is controlled by an animCtrl.
	override def applyTransform_partial_runThrd(xform: MaybeTransform3D): Unit = {
		debug0("applyTransform_runThrd is calling cancelAnyOldAnims")
		cancelAnyOldAnims() // If we don't do this, then the direct transform fails.  We don't expect any COMPLETE
				// events to be delivered as a result of the cancel.

		super.applyTransform_partial_runThrd(xform)

		// Before, when we were doing setEnabled(false) on the animControl, then we needed to reattach like so.
		//	attachAnimCtrl() // If we don't do this, then subsequent bone aninations don't work.
		// Our bone-anim system doesn't go directly through the animCtrl system, but the latter must be
		// involved somehow.
		// If the detach step does not clear old anims, then this reattach will nullify our direct movement,
		// as the animCtrl reasserts control over position of the spatial.
	}
	// applyTransform_runThrd cannot be used reliably on a spatial that is controlled by an animCtrl.
	override def applyTransform_full_runThrd(xform: Transform3D): Unit = {
		debug0("applyTransform_runThrd is calling cancelAnyOldAnims")
		cancelAnyOldAnims() // If we don't do this, then the direct transform fails.  We don't expect any COMPLETE
		// events to be delivered as a result of the cancel.

		super.applyTransform_full_runThrd(xform)
	}
	private def cancelAnyOldAnims(): Unit = {
		val spat = getMainSpat
		val acHelper = new JmeAnimCtrlWrap {}
		acHelper.cancelAnyOldAnimsFound(spat)
	}
	/*
	private def attachAnimCtrl(): Unit = {
		val spat = getMainSpat
		val acHelper = new JmeAnimCtrlWrap {}
		acHelper.ensureAnimCtrlEnabled(spat)
	}
	*/

	protected def applySmooveNow_anyThrd(manipStory: SmooveManipStory, ch : ManipCompletionHandle): Unit = {
		// Since this happens at the "controls" level, is it then not really a sceneGraph mod, requiring queueing?
		val flag_useFullXforms = manipStory.getFlag_useFullXforms
		val anim = myAnimMaker.makeJmeAnimUsingFactory(manipStory, flag_useFullXforms)
		val spat = getMainSpat
		val wrappedMCH = new WrappedMCH(anim.getName, ch)
		applyAnim_anyThrd(anim, spat, wrappedMCH)
	}

	def applySmooveFromCurrent_mystThrd(manipEnding: SmooveManipEnding, ch : ManipCompletionHandle ): Unit = {
		val smv = if (manipEnding.getFlag_useFullXforms) {
			createSmooveStartingFromCurrentFullXform_anyThrd(manipEnding.getXform_finish_full, manipEnding.getDuration_sec)
		} else {
			createSmooveStartingFromCurrentPartialXform_anyThrd(manipEnding.getXform_finish_partial, manipEnding.getDuration_sec)
		}
		info1("Created smoove starting from current pos: {}", smv)
		applySmooveNow_anyThrd(smv, ch )
	}

	private def createSmooveStartingFromCurrentFullXform_anyThrd(endXform: Transform3D, durSec: Float): SmooveManipStory = {
		val currXform = getCurrXform_anyThrd
		val manipFull = new SmooveManipStoryFullImpl(currXform, endXform, durSec)
		manipFull
	}
	private def createSmooveStartingFromCurrentPartialXform_anyThrd(endXform_partial: MaybeTransform3D, durSec: Float): SmooveManipStory = {
		// Two possible approaches should have almost equivalent results, but they don't!
		val jmeAnimBuilderWantsFullness : Boolean = true
		val currXform_full = getCurrXform_anyThrd
		val currXform_partial = if (jmeAnimBuilderWantsFullness) currXform_full else currXform_full.filterByOther(endXform_partial)
		val augmentedEndXform_partial = if (jmeAnimBuilderWantsFullness) endXform_partial.augmentWithDefaults(currXform_full) else endXform_partial
		val manipStory = new SmooveManipStoryPartialImpl(currXform_partial, augmentedEndXform_partial, durSec)
		manipStory
	}
	private def applyAnim_anyThrd(a: JmeGrossAnim, s: Spatial, ch : ManipCompletionHandle): Unit = {
		val acHelper = new JmeAnimCtrlWrap {}
		// acHelper.registerPropagator(s, ch)
		acHelper.fireAnimUnloopedUnblended(s, a, Option(ch))
	}
}
