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
package org.friendularity.vwimpl

import com.jme3.animation.{AnimControl => JmeAnimCtrl, Animation => JmeGrossAnim, AnimEventListener => JmeAnimEventListener, AnimChannel, LoopMode, AnimationFactory}
import com.jme3.math.{Quaternion, Vector3f, ColorRGBA}
import com.jme3.scene.{Node => JmeNode, CameraNode, Spatial}
import org.appdapter.core.name.Ident
import org.appdapter.fancy.log.VarargsLogging
import org.friendularity.vwmsg.{SmooveManipStoryPartialImpl, MaybeTransform3D, ManipCompletionHandle, VWShapeManipRq, ShapeManipRqImpl, SmooveManipStoryFullImpl, SmooveManipEnding, AbruptManipAbs, ManipDesc, DoTransformAbsoluteNow, TransformParams3D, Transform3D, SmooveManipStory}

/**
  * Created by Stub22 on 6/30/2016.
  * Capturing the most important traits needed for smooth anim and transform of given spatials,
  * using JME features, primarily via the class com.jme3.animation.AnimationFactory.
  */

trait VWSmoove

trait HasMainSpat {
	def getMainSpat : Spatial
}
trait HasMainGeom {

}
trait Pickable { // Picked = Selected by user through some UI.
	def notifyPickedState_mystThrd(isPicked : Boolean) : Unit // Notice allows us to display specially
}
trait Addressable {
	def getID : Ident
}
trait Colorable extends HasMainGeom {
	def applyColor_rendThrd(crgba : ColorRGBA)
}
trait Locatable extends HasMainSpat {
	// Reads but doesn't disturb VW scene graph, so OK on any thrd.
	def getCurrXform_anyThrd: Transform3D = {
		val spat = getMainSpat
		val locXform = spat.getLocalTransform
		val locPos = locXform.getTranslation
		val locRot = locXform.getRotation
		val locScl = locXform.getScale

		val xform3D = new TransformParams3D(locPos, locRot, locScl)
		xform3D
	}

}

trait Movable extends HasMainSpat {

	def applyTransform_partial_runThrd(xform : MaybeTransform3D) : Unit = {
		// This impl does not worry about any existing animCtrl.
		// Override (in Smoovable or other) to protect from animCtrls, or to use them.
		naiveTransform_partial_runThrd(xform)
	}
	def applyTransform_full_runThrd(xform : Transform3D) : Unit = {
		// This impl does not worry about any existing animCtrl.
		// Override (in Smoovable or other) to protect from animCtrls, or to use them.
		naiveTransform_full_runThrd(xform)
	}

	protected def naiveTransform_full_runThrd(xform : Transform3D) : Unit = {
		val spat = getMainSpat
		val fPos = xform.getPos
		val fRotQuat = xform.getRotQuat
		val fScale = xform.getScale
		spat.setLocalTranslation(fPos)
		spat.setLocalRotation(fRotQuat)
		spat.setLocalScale(fScale)
	}
	protected def naiveTransform_partial_runThrd(xform : MaybeTransform3D) : Unit = {
		val spat = getMainSpat
		val fPos_opt = xform.getPos_opt
		val fRotQuat_opt = xform.getRotQuat_opt
		val fScale_opt = xform.getScl_opt
		fPos_opt.map(spat.setLocalTranslation(_))
		fRotQuat_opt.map(spat.setLocalRotation(_))
		fScale_opt.map(spat.setLocalScale(_))
	}

	private def rotToLookAtWorldPos_UNUSED(dirToLook_worldCoord : Vector3f, upDir : Vector3f) : Unit = {
		// Fun JME shortcut which we don't currently use, but want to keep in mind at this level.
		// From the source of Spatial.java
		// " Unlike {@link Quaternion#lookAt(com.jme3.math.Vector3f, com.jme3.math.Vector3f) }
		// * this method takes a world position to look at and not a relative direction.
		//
		getMainSpat.lookAt(dirToLook_worldCoord, upDir)
	}

	private def UNUSED_applyTransformAbs_runThrd(xformAbs : DoTransformAbsoluteNow) : Unit = {
		applyTransform_partial_runThrd(xformAbs.getAbsXform)
	}
}

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
		info1("********** Preparing to fire grossAnim={} - canceling old anims and clearing listeners, first", a)
		cancelOldAnims(ac)  // Note that this calls clearChannels
		ac.clearListeners()
		cmplHndl_opt.map(registerPropagator(ac, _))
		val blendTimeSec = 0f
		fireAnim(ac, a, blendTimeSec, LoopMode.DontLoop)
		info1("Completed firing of grossAnim={}", a)
	}
	def fireAnimUnloopedUnblended(s : Spatial, a: JmeGrossAnim, cmplHndl_opt : Option[ManipCompletionHandle]) : Unit = {
		val ac = findOrMakeAnimCtrl(s)
		fireAnimUnloopedUnblended(ac, a, cmplHndl_opt)
	}

	def registerListener(ac : JmeAnimCtrl, ael : JmeAnimEventListener): Unit = {
		ac.addListener(ael)
	}

	def registerPropagator(ac : JmeAnimCtrl, cmplHndl : ManipCompletionHandle) : Unit = {
		val propagator = new CompletionPropagator(cmplHndl)
		registerListener(ac, propagator)
	}
	def registerPropagator(s : Spatial, cmplHndl : ManipCompletionHandle) : Unit = {
		val ac = findOrMakeAnimCtrl(s)
		registerPropagator(ac, cmplHndl)
	}

}
class CompletionPropagator(cmplHndl : ManipCompletionHandle) extends JmeAnimEventListener with VarargsLogging {
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
trait Smoovable extends Movable with Locatable with Addressable with VarargsLogging {
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

trait Manipable extends Smoovable with IdentHlp with VarargsLogging {
	def applyManipDesc(manip : ManipDesc, enqHelp : FullEnqHlp, ch_opt: Option[ManipCompletionHandle]) : Unit = {
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


/*  Inefficient RAM use in AnimationFactory - we could do a tighter  MotionTrack creator than this:
https://github.com/jMonkeyEngine/jmonkeyengine/blob/master/jme3-core/src/main/java/com/jme3/animation/AnimationFactory.java
        totalFrames = (int) (fps * duration) + 1;
        tpf = 1 / (float) fps;
        times = new float[totalFrames];
        translations = new Vector3f[totalFrames];
        rotations = new Quaternion[totalFrames];
        scales = new Vector3f[totalFrames];
 */
trait JmeBoneAnimHlp {
	// Rendering happens on callbacks to the tracks that look like:
	// void setTime(float time, float weight, AnimControl control, AnimChannel channel, TempVars vars);
	// BoneTrack constructor is:
	// BoneTrack(int targetBoneIndex, float[] times, Vector3f[] translations, Quaternion[] rotations, Vector3f[] scales)
	// While SpatialTrack constructor is:
	// SpatialTrack(float[] times, Vector3f[] translations, Quaternion[] rotations, Vector3f[] scales)
	// It does:
	/*
		tempQ.nlerp(tempQ2, blend);
		tempV.interpolateLocal(tempV2, blend);
		tempS.interpolateLocal(tempS2, blend);
	 */
}


// Other main tricks for rot of a spatial are shown in JME Cine MotionEvent.computeTargetDirection, such as
// directly instructing the spatial to look at a Vector3f lookAt, in *world* coordinates.    Reviewing the
// code indicates
// that it just sets the localRot once, does not establish a lower constraint.
// Also note that lookAt is in *world* coordinates.

// spatial.lookAt(lookAt, upVector);

// ...or instead creating a compound quaternion rotation to make spat to point at an offset from the lookAt.
//
// q2.lookAt(direction, upVector);
// q2.multLocal(rotation);
// Quaternion q2 = new Quaternion();
// spatial.setLocalRotation(q2);

// See:   https://github.com/jMonkeyEngine/jmonkeyengine/blob/master/jme3-core/src/main/java/com/jme3/cinematic/events/MotionEvent.java
