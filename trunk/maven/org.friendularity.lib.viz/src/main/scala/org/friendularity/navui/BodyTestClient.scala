package org.friendularity.navui

import org.appdapter.fancy.log.VarargsLogging
import org.friendularity.cpump.CPStrongTeller
import org.friendularity.vwmsg.{VWExoBodyChance, VWBodyMoveRq, VWBodyRq, VWBodyNotice}

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
		val moveRq: VWBodyRq = new VWBodyMoveRq(xf, yf, zf)
		info2("Moving body via teller={}.   Sending moveRq={}", bodyTeller, moveRq)
		bodyTeller.tellStrongCPMsg(moveRq)

		myMoveCounter = if (myMoveCounter == 9) 0 else myMoveCounter + 1

	}
}
