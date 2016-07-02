package org.friendularity.vwimpl

import com.jme3.animation.{LoopMode, AnimControl, AnimationFactory, Animation}
import com.jme3.math.{Quaternion, Vector3f, ColorRGBA}
import com.jme3.scene.Spatial
import org.appdapter.core.name.Ident
import org.friendularity.vwmsg.{VWShapeManipRq, ShapeManipRqImpl, SmooveManipFullImpl, SmooveManipEnding, AbruptManipAbs, ManipDesc, DoTransformAbsoluteNow,  TransformParams3D, Transform3D, SmooveManipFull}

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
		val fPos = new Vector3f
		val fRotQuat = new Quaternion
		val fScale = new Vector3f

		val xform3D = new TransformParams3D(fPos, fRotQuat, fScale)
		xform3D
	}
	//

}

trait Movable extends HasMainSpat {

	def applyTransform_runThrd(xform : Transform3D) : Unit = {
		val spat = getMainSpat
		val fPos = xform.getPos
		val fRotQuat = xform.getRotQuat
		val fScale = xform.getScale
		spat.setLocalTranslation(fPos)
		spat.setLocalRotation(fRotQuat)
		spat.setLocalScale(fScale)
	}

	def rotToLookAtWorldPos_UNUSED(dirToLook_worldCoord : Vector3f, upDir : Vector3f) : Unit = {
		// Fun JME shortcut which we don't currently use, but want to keep in mind at this level.
		// From the source of Spatial.java
		// " Unlike {@link Quaternion#lookAt(com.jme3.math.Vector3f, com.jme3.math.Vector3f) }
		// * this method takes a world position to look at and not a relative direction.
		//
		getMainSpat.lookAt(dirToLook_worldCoord, upDir)
	}

	def applyTransformAbs_runThrd(xformAbs : DoTransformAbsoluteNow) : Unit = {
		applyTransform_runThrd(xformAbs.getAbsXform)
	}
}

trait Smoovable extends Movable with Locatable with Addressable {

	def applySmooveNow_anyThrd(manipFull : SmooveManipFull): Unit = {
		// Since this happens at the "controls" level, is it then not really a sceneGraph mod, requiring queueing?
		val anim = makeJmeAnimUsingFactory(manipFull)
		val spat = getMainSpat
		applyAnim_anyThrd(anim, spat)
	}
	def applySmooveFromCurrent_mystThrd(manipEnding : SmooveManipEnding) : Unit = {
		val smv = createSmooveStartingFromCurrentPos_anyThrd(manipEnding.getXform_finish,
			manipEnding.getDuration_sec)
		applySmooveNow_anyThrd(smv)
	}

	def createSmooveStartingFromCurrentPos_anyThrd(endXform : Transform3D, durSec : Float) : SmooveManipFull = {
		val currXform = getCurrXform_anyThrd
		val manipFull = new SmooveManipFullImpl(currXform, endXform, durSec)
		manipFull
	}


	def makeJmeAnimUsingFactory (smv : SmooveManipFull): Animation = {

		val durF = smv.getDuration_sec
		val animName = "smv_" + smv.hashCode() + "_at_" + System.currentTimeMillis()
		val animFactory = new AnimationFactory(durF, animName)
		addJmeAnimKeyFrame(animFactory, 0, smv.getXform_begin)
		addJmeAnimTimedFrame(animFactory, durF, smv.getXform_finish)
		val jmeAnim = animFactory.buildAnimation()
		jmeAnim
	}
	// This is an jme.AnimFactory-KeyFrame, not the KeyFrame found in jme.cinematic.
	def addJmeAnimKeyFrame(af : AnimationFactory, frameTimeIdx  : Int, frameXform : Transform3D) : Unit = {

		val fPos = frameXform.getPos
		val fRotQuat = frameXform.getRotQuat
		val fScale = frameXform.getScale
		af.addKeyFrameTranslation(frameTimeIdx, fPos)
		af.addKeyFrameRotation(frameTimeIdx, fRotQuat)
		af.addKeyFrameScale(frameTimeIdx, fScale)
	}
	def addJmeAnimTimedFrame(af : AnimationFactory, frameTime  : Float, frameXform : Transform3D) : Unit = {

		val fPos = frameXform.getPos
		val fRotQuat = frameXform.getRotQuat
		val fScale = frameXform.getScale
		af.addTimeTranslation(frameTime, fPos)
		af.addTimeRotation(frameTime, fRotQuat)
		af.addTimeScale(frameTime, fScale)
	}

	def applyAnim_anyThrd(a : Animation, s : Spatial) : Unit = {
		val ctrl = new AnimControl()
		ctrl.addAnim(a)
		s.addControl(ctrl)
		val chan = ctrl.createChannel
		val blendTime = 0f
		chan.setAnim(a.getName, blendTime) // This step "activates" the ctrl-chan-anim combo, yes?
		chan.setLoopMode(LoopMode.DontLoop)  // Cogchar lore says this should be done after setAnim
	}
}
trait Manipable extends Smoovable {
	def applyManipDesc(manip : ManipDesc, enqHelp : FullEnqHlp) : Unit = {
		manip match {
			case smf : SmooveManipFull => {
				applySmooveNow_anyThrd(smf)
			}
			case sme : SmooveManipEnding => {
				applySmooveFromCurrent_mystThrd(sme)
			}
			case ama : AbruptManipAbs => {
				val xform = ama.getXform_finish
				val func : Function0[Unit] = () => { applyTransform_runThrd(xform)}
				enqHelp.enqueueJmeCallable(func)

			}
		}
	}

}
// Other main tricks for rot of a spatial are shown in JME Cine MotionEvent.computeTargetDirection, such as
// directly instructing the spatial to look at a Vector3f lookAt, in *world* coordinates.    Reviewing the
// code indicates
// that it just sets the localRot once, does not establish a lower constraint.
// Also ote that lookAt is in *world* coordinates.

// spatial.lookAt(lookAt, upVector);

// ...or instead creating a compound quaternion rotation to make spat to point at an offset from the lookAt.
//
// q2.lookAt(direction, upVector);
// q2.multLocal(rotation);
// Quaternion q2 = new Quaternion();
// spatial.setLocalRotation(q2);

// See:   https://github.com/jMonkeyEngine/jmonkeyengine/blob/master/jme3-core/src/main/java/com/jme3/cinematic/events/MotionEvent.java
