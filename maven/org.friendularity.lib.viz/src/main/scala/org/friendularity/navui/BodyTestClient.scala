package org.friendularity.navui

import com.jme3.math.{Quaternion, Vector3f}
import org.appdapter.fancy.log.VarargsLogging
import org.friendularity.cpmsg.CPStrongTeller
import org.friendularity.vwmsg.{VWBodyManipRq, AbruptManipAbsImpl, TransformParams3D, VWExoBodyChance, VWBodyRq, VWBodyNotice}

/**
  * Created by Owner on 6/18/2016.
  */
trait BodyTestClient extends VarargsLogging {
	def makeMoveTestLogic(): ExoBodyUserLogic = {
		val userLogic: ExoBodyUserLogic = new ExoBodyUserLogic() {
			override protected def rcvUpdtickForBody(exoBodyUpChance : VWExoBodyChance, bodyTeller : CPStrongTeller[VWBodyRq]) : Unit = {
				moveThatBody(bodyTeller)
			}
		}
		userLogic
	}
	var myMoveCounter : Int = 0
	def moveThatBody(bodyTeller: CPStrongTeller[VWBodyRq]) : Unit = {
		val xf = myMoveCounter * 2.5f
		val yf = (myMoveCounter / 5) * 4.0f
		val zf = (myMoveCounter % 2) * 1.0f
		// val moveRq: VWBodyRq = new VWBodyMoveRq(xf, yf, zf)
		val tgtPosVec = new Vector3f(xf, yf, zf)
		val xform = new TransformParams3D(tgtPosVec, Quaternion.IDENTITY, Vector3f.UNIT_XYZ)
		val manip = new AbruptManipAbsImpl(xform)
		val manipRq = new VWBodyManipRq(manip)
		debug2("Moving body via teller={}.   Sending manipRq={}", bodyTeller, manipRq)
		bodyTeller.tellStrongCPMsg(manipRq)

		myMoveCounter = if (myMoveCounter == 9) 0 else myMoveCounter + 1

	}
}
