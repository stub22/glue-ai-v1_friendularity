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
	def getXform_finish : Transform3D
}
trait HasBeginXform3D {
	def getXform_begin : Transform3D
}
trait ManipStatusMsg extends CPumpMsg {
	def getStatusCode : Int
	val	MANIP_COMPLETE	: Int = 1
	val	MANIP_ABORTED	= 2

	def getHandleID : Ident
}
case class ManipStatus_Complete(statusHandleID : Ident, dbgBonus : Any) extends ManipStatusMsg {
	override def getStatusCode : Int = MANIP_COMPLETE
	override def getHandleID : Ident = statusHandleID
}
trait ManipCompletionHandle {
	def notifyComplete(dbgBonus : Any) : Unit
	def getHandleID : Ident
}
// def getManipStatusTeller_opt : Option[CPStrongTeller[ManipStatusMsg]] = None
// def getManipID : Ident = ???

trait ManipDesc  /*extends  ManipCompletionHandle with VarargsLogging {
	protected def getDelegateHandle_opt : Option[ManipCompletionHandle] = None
	override def notifyComplete : Unit = {

	}
} */

trait AbruptManipAbs extends ManipDesc with HasFinishXform3D // Abruptly moves the target to the finishXform

trait SmooveManipEnding extends ManipDesc with HasFinishXform3D with HasDuration
trait SmooveManipFull extends ManipDesc with HasBeginXform3D with HasFinishXform3D with HasDuration

class ManipStatusPropagator(statTlrOpt : Option[CPStrongTeller[ManipStatusMsg]]) extends ManipCompletionHandle with IdentHlp {
	def getManipStatusTeller_opt : Option[CPStrongTeller[ManipStatusMsg]] = statTlrOpt
	private val myPropagatorID = makeStampyRandyIdent("mnpStatPrpg")
	override def getHandleID : Ident = myPropagatorID
	override def notifyComplete(dbgBonus : Any) : Unit = {
		val statTeller = getManipStatusTeller_opt
		statTeller.map( tlr => {
			val msg = ManipStatus_Complete(getHandleID, dbgBonus)
			tlr.tellStrongCPMsg(msg)
		})
	}
	override def toString = "ManipStatusPropagator[handleID=" + getHandleID + "]"
}
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
