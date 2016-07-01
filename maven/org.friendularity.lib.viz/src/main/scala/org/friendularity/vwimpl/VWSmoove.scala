package org.friendularity.vwimpl

import com.jme3.animation.{LoopMode, AnimControl, AnimationFactory, Animation}
import com.jme3.math.{Quaternion, Vector3f, ColorRGBA}
import com.jme3.scene.Spatial
import org.appdapter.core.name.Ident
import org.friendularity.vwmsg.{SmooveFromCurrent3D, SmooveFullImpl, TransformParams3D, Transform3D, Smoove3D}

/**
  * Created by Owner on 6/30/2016.
  */

trait VWSmoove

trait HasMainSpat {
	def getMainSpat : Spatial
}
trait HasMainGeom {

}
trait Pickable {
	def notifyPicked_mystThrd : Unit
}
trait Addressable {
	def getID : Ident
}
trait Colorable extends HasMainGeom {
	def applyColor_mystThrd(crgba : ColorRGBA)
}
trait Locatable extends HasMainSpat {
	def getCurrXform_anyThrd: Transform3D = {
		val fPos = new Vector3f
		val fRotQuat = new Quaternion
		val fScale = new Vector3f

		val xform3D = new TransformParams3D(fPos, fRotQuat, fScale)
		xform3D
	}
}

trait Movable extends HasMainSpat {
	def applyTransform_mystThrd(xform : Transform3D) : Unit = {
		val spat = getMainSpat
		val fPos = xform.getPos
		val fRotQuat = xform.getRotQuat
		val fScale = xform.getScale
		spat.setLocalTranslation(fPos)
		spat.setLocalRotation(fRotQuat)
		spat.setLocalScale(fScale)
	}
}

trait Smoovable extends Movable with Locatable with Addressable {

	def applySmooveNow_anyThrd(smv : Smoove3D): Unit = {
		// Since this happens at the "controls" level, is it then not really a sceneGraph mod, requiring queueing?
		val anim = makeJmeAnimUsingFactory(smv)
		val spat = getMainSpat
		applyAnim(anim, spat)
	}

	def createSmooveStartingFromCurrentPos_anyThrd(endXform : Transform3D, durSec : Float) : Smoove3D = {
		val currXform = getCurrXform_anyThrd
		val smv = new SmooveFullImpl(getID, currXform, endXform, durSec)
		smv
	}

	def applySmooveFromCurrent_mystThrd(smvFrmCur : SmooveFromCurrent3D) : Unit = {
		val smv = createSmooveStartingFromCurrentPos_anyThrd(smvFrmCur.getXform_finish, smvFrmCur.getDuration_sec)
		applySmooveNow_anyThrd(smv)
	}

	def makeJmeAnimUsingFactory (smv : Smoove3D): Animation = {

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

	def applyAnim(a : Animation, s : Spatial) : Unit = {
		val ctrl = new AnimControl()
		ctrl.addAnim(a)
		s.addControl(ctrl)
		val chan = ctrl.createChannel
		val blendTime = 0f
		chan.setAnim(a.getName, blendTime) // This step "activates" the ctrl-chan-anim combo, yes?
		chan.setLoopMode(LoopMode.DontLoop)
	}
}
// Here's some apparent clarity, from 2014, on AnimFactory, MotionPath, MotionEvent
//
// https://hub.jmonkeyengine.org/t/playing-motionevent-and-animationcontrol-at-the-same-time-with-a-cinematic/30709/2

// Note Nehon's suggestion that MotionEvent be added as a control







