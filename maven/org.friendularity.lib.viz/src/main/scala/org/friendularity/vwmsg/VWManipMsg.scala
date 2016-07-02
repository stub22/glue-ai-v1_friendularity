package org.friendularity.vwmsg

import com.jme3.math.{Quaternion, Vector3f}
import org.appdapter.core.name.Ident

/**
  * Created by Owner on 7/2/2016.
  */
// One conceptual difficulty is demarking between "no value" and "default value", or "leave unchanged" vs "reset"
trait Located3D {
	def getPos : Vector3f
}
trait Rotated3D {
	def getRotQuat : Quaternion
}
trait Scaled3D {
	def getScale : Vector3f
}
trait Transform3D extends Located3D with Rotated3D with Scaled3D

// Different impls of Transform3D
// Fancier:  Wrap subunits, which can be easier if computation is nontrivial
class CompoundXform3D(loc: Located3D, rot: Rotated3D, scale: Scaled3D) extends Transform3D {
	override def getPos : Vector3f = loc.getPos
	override def getRotQuat: Quaternion = rot.getRotQuat
	override def getScale: Vector3f = scale.getScale
}
// ...OR just supply some nice known scalar values.
class TransformParams3D(myPos3f : Vector3f, myRotQuat : Quaternion, myScale3f : Vector3f) extends Transform3D {
	override def getPos : Vector3f = myPos3f
	override def getRotQuat : Quaternion = myRotQuat
	override def getScale : Vector3f = myScale3f
}

trait HasDuration {
	def getDuration_sec : Float
	def getDuration_millisec : Int
}
trait HasFinishXform3D {
	def getXform_finish : Transform3D
}
trait HasBeginXform3D {
	def getXform_begin : Transform3D
}
trait ManipDesc

trait AbruptManipAbs extends ManipDesc with HasFinishXform3D // Abruptly moves the target to the finishXform

trait SmooveManipEnding extends ManipDesc with HasFinishXform3D with HasDuration
trait SmooveManipFull extends ManipDesc with HasBeginXform3D with HasFinishXform3D with HasDuration

class SmooveManipGutsImpl(finishXForm : Transform3D, durSec : Float) extends HasFinishXform3D with HasDuration {
	override def getDuration_sec : Float = durSec
	override def getDuration_millisec : Int = Math.round(durSec * 1000.0f)
	override def getXform_finish : Transform3D = finishXForm
}

case class SmooveManipEndingImpl(finishXForm : Transform3D, durSec : Float) extends SmooveManipGutsImpl(finishXForm,
	durSec) with SmooveManipEnding

case class SmooveManipFullImpl(beginXform : Transform3D, finishXForm : Transform3D, durSec : Float)
			extends SmooveManipGutsImpl(finishXForm, durSec) with SmooveManipFull {
	override def getXform_begin : Transform3D = beginXform
}
