package org.friendularity.vwmsg

import com.jme3.math.{Quaternion, Vector3f}
import org.appdapter.core.name.Ident
import org.appdapter.fancy.log.VarargsLogging
import org.friendularity.cpmsg.{CPumpMsg, CPStrongTeller}
import org.friendularity.vwimpl.IdentHlp

/**
  * Created by Stub22  on 7/2/2016.
  */


trait HasDuration {
	def getDuration_sec : Float
	def getDuration_millisec : Int
}
trait HasFinishXform3D {
	def getXform_finish_partial : MaybeTransform3D
	def getXform_finish_full : Transform3D
}
trait HasBeginXform3D {
	def getXform_begin_partial : MaybeTransform3D
	def getXform_begin_full : Transform3D
}
trait ManipStatusMsg extends CPumpMsg {
	def getStatusCode : Int
	val	MANIP_COMPLETE	: Int = 1
	val	MANIP_ABORTED	= 2

	def getHandleID : Ident

	def getAnimName_opt : Option[String]
}
case class ManipStatus_Complete(statusHandleID : Ident, animName_opt : Option[String], dbgBonus : Any) extends ManipStatusMsg {
	override def getStatusCode : Int = MANIP_COMPLETE
	override def getHandleID : Ident = statusHandleID
	override def getAnimName_opt : Option[String] = animName_opt
}
trait ManipCompletionHandle {
	def notifyComplete(animName : String, dbgBonus : Any) : Unit
	def getHandleID : Ident
}
// def getManipStatusTeller_opt : Option[CPStrongTeller[ManipStatusMsg]] = None
// def getManipID : Ident = ???

trait ManipDesc  {  // extends  ManipCompletionHandle with VarargsLogging
	def getFlag_useFullXforms : Boolean = false
//	protected def getDelegateHandle_opt : Option[ManipCompletionHandle] = None
//	override def notifyComplete : Unit = {
}
trait FullManipDesc extends ManipDesc {
	override def getFlag_useFullXforms : Boolean = true
}

trait AbruptManipAbs extends ManipDesc with HasFinishXform3D // Abruptly moves the target to the finishXform
trait SmooveManipEnding extends ManipDesc with HasFinishXform3D with HasDuration
trait SmooveManipStory extends ManipDesc with HasBeginXform3D with HasFinishXform3D with HasDuration

class ManipStatusPropagator(statTlrOpt : Option[CPStrongTeller[ManipStatusMsg]]) extends ManipCompletionHandle with IdentHlp {
	def getManipStatusTeller_opt : Option[CPStrongTeller[ManipStatusMsg]] = statTlrOpt
	private val myPropagatorID = makeStampyRandyIdent("mnpStatPrpg")
	override def getHandleID : Ident = myPropagatorID
	override def notifyComplete(animName : String, dbgBonus : Any) : Unit = {
		val statTeller = getManipStatusTeller_opt
		statTeller.map( tlr => {
			val msg = ManipStatus_Complete(getHandleID, Option(animName), dbgBonus)
			tlr.tellStrongCPMsg(msg)
		})
	}
	override def toString = "ManipStatusPropagator[handleID=" + getHandleID + "]"
}
case class AbruptManipAbsFullImpl(xform_full : Transform3D) extends AbruptManipAbs with FullManipDesc {
	override def getXform_finish_full : Transform3D = xform_full
	override def getXform_finish_partial : MaybeTransform3D = xform_full
}
case class AbruptManipAbsPartialImpl(xform_partial : MaybeTransform3D) extends AbruptManipAbs with MakesTransform3D {
	override def getXform_finish_full : Transform3D = makeDefiniteXForm(xform_partial)
	override def getXform_finish_partial : MaybeTransform3D = xform_partial
}
class SmooveManipGutsFullImpl(finishXForm_full : Transform3D, durSec : Float) extends HasFinishXform3D with HasDuration {
	override def getDuration_sec : Float = durSec
	override def getDuration_millisec : Int = Math.round(durSec * 1000.0f)
	override def getXform_finish_full : Transform3D = finishXForm_full
	override def getXform_finish_partial : MaybeTransform3D = finishXForm_full
}

case class SmooveManipEndingFullImpl(finishXForm_full : Transform3D, durSec : Float) extends SmooveManipGutsFullImpl(finishXForm_full,
	durSec) with SmooveManipEnding with FullManipDesc


case class SmooveManipEndingPartialImpl(finishXForm_part : MaybeTransform3D, durSec : Float) extends HasFinishXform3D with HasDuration
			with SmooveManipEnding with MakesTransform3D {
	override def getXform_finish_partial : MaybeTransform3D = finishXForm_part
	override def getXform_finish_full : Transform3D = makeDefiniteXForm(finishXForm_part)

	override def getDuration_sec : Float = durSec
	override def getDuration_millisec : Int = Math.round(durSec * 1000.0f)
}


case class SmooveManipStoryFullImpl(beginXform_full : Transform3D, finXForm_full : Transform3D, durSec : Float)
			extends SmooveManipGutsFullImpl(finXForm_full, durSec) with SmooveManipStory with FullManipDesc {
	override def getXform_begin_full : Transform3D = beginXform_full
	override def getXform_begin_partial : MaybeTransform3D = beginXform_full


}
case class SmooveManipStoryPartialImpl(beginXform_part : MaybeTransform3D, finishXForm_part : MaybeTransform3D, durSec : Float)
			extends HasDuration with SmooveManipStory with MakesTransform3D {
	override def getXform_begin_partial : MaybeTransform3D = beginXform_part
	override def getXform_begin_full : Transform3D = makeDefiniteXForm(beginXform_part)

	override def getXform_finish_partial : MaybeTransform3D = finishXForm_part
	override def getXform_finish_full : Transform3D = makeDefiniteXForm(finishXForm_part)

	override def getDuration_sec : Float = durSec
	override def getDuration_millisec : Int = Math.round(durSec * 1000.0f)

	override def getFlag_useFullXforms : Boolean = false  // Same as inherited value, but we override for clarity
}
