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

import com.jme3.animation.{AnimChannel, AnimControl => JmeAnimCtrl, AnimEventListener => JmeAnimEventListener, Animation => JmeGrossAnim, AnimationFactory, LoopMode}
import org.appdapter.fancy.log.VarargsLogging
import org.friendularity.vw.mprt.manip.{MaybeTransform3D, Transform3D, SmooveManipStory}

/**
  * Code moved to new file on 1/19/2017.
  */
trait JmeAnimMaker extends VarargsLogging {
	def makeJmeAnimUsingFactory (smv : SmooveManipStory, assumeFullXforms : Boolean): JmeGrossAnim = {
		// Hmmm, the animFactory creates all frames in memory when it creates a SpatialTrack.
		// SpatialTrack does have a times[] array, but it is used very bluntly by AnimFactory.
		// It looks like if we made SpatialTracks directly, they could use much less RAM.

		val durF = smv.getDuration_sec
		val animName = "smv_" + smv.hashCode() + "_at_" + System.currentTimeMillis()
		val animFactory = new AnimationFactory(durF, animName)
		if (assumeFullXforms) {
			info1("Using full keyFrames for story: {}", smv)
			addFullJmeAnimKeyFrame(animFactory, 0, smv.getXform_begin_full)
			addFullJmeAnimTimedFrame(animFactory, durF, smv.getXform_finish_full)
		} else {
			info1("Using partial keyFrames for story: {}", smv)
			addPartialJmeAnimKeyFrame(animFactory, 0, smv.getXform_begin_partial)
			addPartialJmeAnimTimedFrame(animFactory, durF, smv.getXform_finish_partial)
		}
		val jmeAnim = animFactory.buildAnimation()
		jmeAnim
	}
	// This is an jme.AnimFactory-KeyFrame, not the KeyFrame found in jme.cinematic.

	// Animation is treated as a seq of interpolated frames.
	// We have variants for both integer index (e.g. 0 is exactly the first frame) and interpolated time.
	// We also can absorb either Full or Partial("Maybe") transforms.
	private def addFullJmeAnimKeyFrame(af : AnimationFactory, frameTimeIdx  : Int, frameXform : Transform3D) : Unit = {

		val fPos = frameXform.getPos
		val fRotQuat = frameXform.getRotQuat
		val fScale = frameXform.getScale
		af.addKeyFrameTranslation(frameTimeIdx, fPos)
		af.addKeyFrameRotation(frameTimeIdx, fRotQuat)
		af.addKeyFrameScale(frameTimeIdx, fScale)
	}
	private def addFullJmeAnimTimedFrame(af : AnimationFactory, frameTime  : Float, frameXform : Transform3D) : Unit = {

		val fPos = frameXform.getPos
		val fRotQuat = frameXform.getRotQuat
		val fScale = frameXform.getScale
		af.addTimeTranslation(frameTime, fPos)
		af.addTimeRotation(frameTime, fRotQuat)
		af.addTimeScale(frameTime, fScale)
	}
	// 2016-11-21 - Stu sez:  Hmmm, this "partial" approach doesn't seem to work, so far.
	// At least, main cam seems to be jumping to a default +z orientation, even though both rotQuats
	// are set to None - meaning the keyframe should have no rot info.  Does Jme anim builder require/assume
	// a complete transform at each keyframe?
	private def addPartialJmeAnimKeyFrame(af : AnimationFactory, frameTimeIdx  : Int, frameXform : MaybeTransform3D) : Unit = {
		frameXform.getPos_opt.map(af.addKeyFrameTranslation(frameTimeIdx, _))
		frameXform.getRotQuat_opt.map(af.addKeyFrameRotation(frameTimeIdx, _))
		frameXform.getScl_opt.map(af.addKeyFrameScale(frameTimeIdx, _))
	}
	private def addPartialJmeAnimTimedFrame(af : AnimationFactory, frameTime  : Float, frameXform : MaybeTransform3D) : Unit = {
		frameXform.getPos_opt.map(af.addTimeTranslation(frameTime, _))
		frameXform.getRotQuat_opt.map(af.addTimeRotation(frameTime, _))
		frameXform.getScl_opt.map(af.addTimeScale(frameTime, _))
	}

}
