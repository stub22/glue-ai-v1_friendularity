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
import org.friendularity.netcli.goodtst.TestManyGoodyBursts
import org.friendularity.vw.mprt.manip.PartialTransform3D

// , TTGRidBurstTest}

/**
  * Created by Stub22 on 9/5/2016.
  */

class ClientTestMsgSender(initDelayMsec : Int, stepDelayMsec : Int,
						  sinbadMoves : Boolean, xtraCam : Boolean,
						  goodyPile : Boolean, mainCamMoves : Boolean,
						  goodyBursts : Boolean) extends OffersVWorldClient with VWTAMsgMaker  {
	override val myPreferredEncoding : Int = myClient.ENCODE_PREF_TRT
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
		info0("ClientTestMsgSender starting sendBunchaMsgs")
		val clientOffer = this
		if (sinbadMoves) {
			val tgtPos = new Vector3f(-20.0f, 90.0f, -20.0f)
			val tgtScl = new Vector3f(12.0f, 3.0f, 8.0f)
			val mxf = new PartialTransform3D(Some(tgtPos), Some(quarterTurn), Some(tgtScl))
			clientOffer.sendSinbadSmooveRq(mxf, 1.5f)
			pauseOneStepDelay
		}
		if (xtraCam) {
			val xtraCamGuideShapeID = clientOffer.makeStampyRandyIdent("xtraCam")
			clientOffer.sendRq_makeExtraCamera(xtraCamGuideShapeID)
			pauseOneStepDelay
			val nextTgtCamPos = new Vector3f(-80.0f, 50.0f, -72.7f)
			val cxf = new PartialTransform3D(Some(nextTgtCamPos), None, None)
			clientOffer.sendRq_smooveCameraGuideShape(xtraCamGuideShapeID, cxf, 20.0f)
			pauseOneStepDelay
		} else {
			pauseOneStepDelay
		}
		if (sinbadMoves) {
			val tgtPos = new Vector3f(-5.0f, 38.0f, 0.6f)
			// val tgtScl = new Vector3f(12.0f, 3.0f, 8.0f)
			val mxf = new PartialTransform3D(Some(tgtPos), None, None) // Some(tgtScl))
			clientOffer.sendSinbadAbruptMoveRq(mxf)
			// clientOffer.sendSinbadSmooveRq(mxf, 1.5f)
			pauseOneStepDelay
		}

		if (goodyPile) {
			// Serves as a basic goody plumbing test, across both turtle and binary-ser encodings.
			val dummyGoodySender = new DummyGoodySender {}
			dummyGoodySender.slowlySendSomeVWRqs(myClient, stepDelayMsec)
		}

		if (sinbadMoves) {
			val nxtChrPos = new Vector3f(40.0f, 10.0f, 20.0f)
			val nxf = new PartialTransform3D(Some(nxtChrPos), Some(oppositeTurn), None)
			clientOffer.sendSinbadSmooveRq(nxf, 8.0f)
		}
		if (goodyBursts) {
			maybeRunBursts
		}
		info0("ClientTestMsgSender finished sendBunchaMsgs")
	}
	def sendMainCamMsg(useSmooves : Boolean): Unit = {
		val clientOffer = this
		val mainCamID = new FreeIdent(LightsCameraAN.URI_defaultCam)
		val nxtCamPos = new Vector3f(5.0f, 33.0f, -100.0f)

		val nxfA = new PartialTransform3D(Some(nxtCamPos), Some(quarterTurn), None)
		val dur = 7.0f

		if (useSmooves) {
			info0("Sending main-cam smoove 1")
			clientOffer.sendRq_smooveRootCamera(mainCamID, nxfA, dur)
		} else {
			clientOffer.sendRq_abruptMoveRootCamera(mainCamID, nxfA)
		}

		pauseOneStepDelay
		val nxtCamPos2 = new Vector3f(-5.0f, 10.0f, -50.0f)
		val nxf2 = new PartialTransform3D(Some(nxtCamPos2), None, None)

		if (useSmooves) {
			info0("Sending main-cam smoove 2")
			clientOffer.sendRq_smooveRootCamera(mainCamID, nxf2, dur)
		} else {
			clientOffer.sendRq_abruptMoveRootCamera(mainCamID, nxf2)
		}
		pauseOneStepDelay

		if (useSmooves) {
			info0("Sending main-cam smoove 3")
			clientOffer.sendRq_smooveRootCamera(mainCamID, nxfA, dur)
		} else {
			clientOffer.sendRq_abruptMoveRootCamera(mainCamID, nxfA)
		}
		pauseOneStepDelay

		if (useSmooves) {
			info0("Sending main-cam smoove 4")
			clientOffer.sendRq_smooveRootCamera(mainCamID, nxf2, dur)
		} else {
			clientOffer.sendRq_abruptMoveRootCamera(mainCamID, nxf2)
		}
	}

	val mainCamSmoove_notAbrupt = true

	private def pauseOneStepDelay : Unit = {
		Thread.sleep(stepDelayMsec)
	}
	def sendAll():  Unit = {
		info1("Client test send thread is sleeping for initDelay={} msec", initDelayMsec: Integer)
		Thread.sleep(initDelayMsec)

		if (mainCamMoves) {
			sendMainCamMsg(mainCamSmoove_notAbrupt)
			pauseOneStepDelay
		}
		sendBunchaMsgs()
		if (mainCamMoves) {
			pauseOneStepDelay
			sendMainCamMsg(mainCamSmoove_notAbrupt)
		}
	}
	val FLAG_burstMode = true
	private def maybeRunBursts : Unit = {
		if (FLAG_burstMode) {
			val clientOffer = this
			val mbt = new TestManyGoodyBursts(clientOffer)
			mbt.fireSomeBursts()
			pauseOneStepDelay
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


		val (doSinbadMoves, doExtraCam, doGoodyPile, doMainCamMoves, doGoodyBursts) = (false, false, true, true, true)
						// (true, true, false, true, true)
		val (initDelayMsec, stepDelayMsec) = (3000, 2000)
		val clientTestSender = new ClientTestMsgSender(initDelayMsec, stepDelayMsec, doSinbadMoves, doExtraCam, doGoodyPile, doMainCamMoves, doGoodyBursts)

		// We do not startThread, because we want client to exit after sending.
		// If we cared about replies/notices, we could start a thread, and watch for the ____ signal to exit.
		clientTestSender.sendAll
	}
}