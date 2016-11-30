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

package org.friendularity.netcli.vwta

import com.jme3.math.{Quaternion, Vector3f}
import org.appdapter.core.name.FreeIdent
import org.cogchar.name.cinema.LightsCameraAN

import org.friendularity.vwmsg.{PartialTransform3D}

/**
  * Created by Stub22 on 9/5/2016.
  */

class ClientTestMsgSender(initDelayMsec : Int, stepDelayMsec : Int, sinbadMoves : Boolean, xtraCam : Boolean,
						  goodyPile : Boolean) extends OffersVWorldClient with VWTAMsgMaker  {
	override val myPreferredEncoding : Int = myClient.ENCODE_PREF_BIN
	val quarterAngle : Float = 0.5f * Math.PI.asInstanceOf[Float]
	val srcQ = new Quaternion
	val quarterTurn : Quaternion = srcQ.fromAngles(0.0f, quarterAngle, 0.0f)
	val oppositeTurn = quarterTurn.inverse

	def initClientConn(): Unit = {
		val clientOffer = this
		info0("========== ClientTestMsgSender.initClientConn() starting CLIENT qpidConn")
		clientOffer.startQpidConn
		clientOffer.checkClient
	}

	protected def sendBunchaMsgs() : Unit = {
		val clientOffer = this
		if (sinbadMoves) {
			val tgtPos = new Vector3f(-20.0f, 90.0f, -20.0f)
			val tgtScl = new Vector3f(12.0f, 3.0f, 8.0f)
			val mxf = new PartialTransform3D(Some(tgtPos), Some(quarterTurn), Some(tgtScl))
			clientOffer.sendSinbadSmooveRq(mxf, 1.5f)
			Thread.sleep(stepDelayMsec)
		}
		if (xtraCam) {
			val xtraCamGuideShapeID = clientOffer.makeStampyRandyIdent("xtraCam")
			clientOffer.sendRq_makeExtraCamera(xtraCamGuideShapeID)
			Thread.sleep(stepDelayMsec)
			val nextTgtCamPos = new Vector3f(-80.0f, 50.0f, -72.7f)
			val cxf = new PartialTransform3D(Some(nextTgtCamPos), None, None)
			clientOffer.sendRq_smooveCameraGuideShape(xtraCamGuideShapeID, cxf, 20.0f)
			Thread.sleep(stepDelayMsec)
		} else {
			Thread.sleep(stepDelayMsec)
		}
		if (sinbadMoves) {
			val tgtPos = new Vector3f(-5.0f, 38.0f, 0.6f)
			// val tgtScl = new Vector3f(12.0f, 3.0f, 8.0f)
			val mxf = new PartialTransform3D(Some(tgtPos), None, None) // Some(tgtScl))
			clientOffer.sendSinbadAbruptMoveRq(mxf)
			// clientOffer.sendSinbadSmooveRq(mxf, 1.5f)
			Thread.sleep(stepDelayMsec)
		}

		if (goodyPile) {
			val dummyGoodySender = new DummyGoodySender {}
			dummyGoodySender.slowlySendSomeVWRqs(myClient, stepDelayMsec)
		}

		if (sinbadMoves) {
			val nxtChrPos = new Vector3f(40.0f, 10.0f, 20.0f)
			val nxf = new PartialTransform3D(Some(nxtChrPos), Some(oppositeTurn), None)
			clientOffer.sendSinbadSmooveRq(nxf, 8.0f)
		}
	}
	def sendMainCamMsg(useSmooves : Boolean): Unit = {
		val clientOffer = this
		val mainCamID = new FreeIdent(LightsCameraAN.URI_defaultCam)
		val nxtCamPos = new Vector3f(5.0f, 33.0f, -250.0f)

		val nxfA = new PartialTransform3D(Some(nxtCamPos), Some(quarterTurn), None)
		val dur = 7.0f

		if (useSmooves) {
			info0("Sending main-cam smoove 1")
			clientOffer.sendRq_smooveRootCamera(mainCamID, nxfA, dur)
		} else {
			clientOffer.sendRq_abruptMoveRootCamera(mainCamID, nxfA)
		}

		Thread.sleep(stepDelayMsec)
		val nxtCamPos2 = new Vector3f(-5.0f, 10.0f, -70.0f)
		val nxf2 = new PartialTransform3D(Some(nxtCamPos2), None, None)

		if (useSmooves) {
			info0("Sending main-cam smoove 2")
			clientOffer.sendRq_smooveRootCamera(mainCamID, nxf2, dur)
		} else {
			clientOffer.sendRq_abruptMoveRootCamera(mainCamID, nxf2)
		}
		Thread.sleep(stepDelayMsec)

		if (useSmooves) {
			info0("Sending main-cam smoove 3")
			clientOffer.sendRq_smooveRootCamera(mainCamID, nxfA, dur)
		} else {
			clientOffer.sendRq_abruptMoveRootCamera(mainCamID, nxfA)
		}
		Thread.sleep(stepDelayMsec)

		if (useSmooves) {
			info0("Sending main-cam smoove 4")
			clientOffer.sendRq_smooveRootCamera(mainCamID, nxf2, dur)
		} else {
			clientOffer.sendRq_abruptMoveRootCamera(mainCamID, nxf2)
		}
	}
/*
	def testMainCamBindAndSmoove: Unit = {
		val clientOffer = this
		val mainCamID = new FreeIdent(LightsCameraAN.URI_defaultCam)

	}
*/
	val testMainCamMoves = false
	val mainCamSmoove_notAbrupt = true

	def sendAll():  Unit = {
		info1("Client test send thread is sleeping for {} msec", initDelayMsec: Integer)
		if (testMainCamMoves) {
			Thread.sleep(initDelayMsec)
			sendMainCamMsg(mainCamSmoove_notAbrupt)
		}
		Thread.sleep(stepDelayMsec)
		sendBunchaMsgs()
		if (testMainCamMoves) {
			Thread.sleep(stepDelayMsec)
			sendMainCamMsg(mainCamSmoove_notAbrupt)
		}
	}
	def startTestThread () {
		initClientConn()
		val testSendThrd = new Thread() {
			override def run: Unit = {
				sendAll()
			}
		}
		testSendThrd.start()
	}

}
object RunClientTestMsgSender {
	def main(args: Array[String]): Unit = {

		val (doSinbadMoves, doExtraCam, doGoodyPile) = (true, true, false)

		val clientTestSender = new ClientTestMsgSender(3000, 2000, doSinbadMoves, doExtraCam, doGoodyPile)
		clientTestSender.sendAll
	}
}