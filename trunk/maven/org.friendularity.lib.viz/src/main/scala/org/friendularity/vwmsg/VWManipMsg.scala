package org.friendularity.vwmsg

import com.jme3.math.{Quaternion, Vector3f}
import org.appdapter.core.name.Ident

/**
  * Created by Stub22  on 7/2/2016.
  */


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

case class AbruptManipAbsImpl(xform : Transform3D) extends AbruptManipAbs {
	override def getXform_finish : Transform3D = xform
}
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
