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

import com.jme3.math.Vector3f
import org.appdapter.core.name.FreeIdent
import org.cogchar.name.cinema.LightsCameraAN

import org.friendularity.vwmsg.{PartialTransform3D}

/**
  * Created by Stub22 on 9/5/2016.
  */

class ClientTestMsgSender(initDelayMsec : Int, stepDelayMsec : Int, sinbadMoves : Boolean, xtraCam : Boolean,
						  goodyPile : Boolean) extends OffersVWorldClient with VWTAMsgMaker  {
	override val myPreferredEncoding : Int = myClient.ENCODE_PREF_BIN

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
			val mxf = new PartialTransform3D(Some(tgtPos), None, Some(tgtScl))
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
			Thread.sleep(stepDelayMsec)
		}

		if (goodyPile) {
			val dummyGoodySender = new DummyGoodySender {}
			dummyGoodySender.slowlySendSomeVWRqs(myClient, stepDelayMsec)
		}

		if (sinbadMoves) {
			val nxtChrPos = new Vector3f(40.0f, 10.0f, 20.0f)
			val nxf = new PartialTransform3D(Some(nxtChrPos), None, None)
			clientOffer.sendSinbadSmooveRq(nxf, 8.0f)
		}
	}
	def sendMainCamMsgs_AbruptDirect(): Unit = {
		val clientOffer = this
		val mainCamID = new FreeIdent(LightsCameraAN.URI_defaultCam)
		val nxtCamPos = new Vector3f(5.0f, 3.0f, 150.0f)
		val nxf = new PartialTransform3D(Some(nxtCamPos), None, None)
		// clientOffer.sendRq_abruptMoveRootCamera(mainCamID, nxf)
		val dur = 7.0f
		clientOffer.sendRq_smooveRootCamera(mainCamID, nxf, dur)
		Thread.sleep(stepDelayMsec)
		val nxtCamPos2 = new Vector3f(-5.0f, 8.0f, 40.0f)
		val nxf2 = new PartialTransform3D(Some(nxtCamPos2), None, None)
//		clientOffer.sendRq_abruptMoveRootCamera(mainCamID, nxf2)
		clientOffer.sendRq_smooveRootCamera(mainCamID, nxf2, dur)

		Thread.sleep(stepDelayMsec)
		//		clientOffer.sendRq_abruptMoveRootCamera(mainCamID, nxf)
		clientOffer.sendRq_smooveRootCamera(mainCamID, nxf, dur)

		Thread.sleep(stepDelayMsec)
		// clientOffer.sendRq_abruptMoveRootCamera(mainCamID, nxf2)
		clientOffer.sendRq_smooveRootCamera(mainCamID, nxf2, dur)
	}
/*
	def testMainCamBindAndSmoove: Unit = {
		val clientOffer = this
		val mainCamID = new FreeIdent(LightsCameraAN.URI_defaultCam)

	}
*/
	val testMainCamAbruptDirect = true
	def sendAll(): Unit = {
		info1("Client test send thread is sleeping for {} msec", initDelayMsec: Integer)
		if (testMainCamAbruptDirect) {
			Thread.sleep(initDelayMsec)
			sendMainCamMsgs_AbruptDirect()
		}
		Thread.sleep(stepDelayMsec)
		sendBunchaMsgs()
		if (testMainCamAbruptDirect) {
			Thread.sleep(stepDelayMsec)
			sendMainCamMsgs_AbruptDirect()
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