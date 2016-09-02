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

import javax.jms.Session

import com.jme3.math.Vector3f
import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.api.thing.ThingActionSpec
import org.cogchar.name.goody.GoodyNames
import org.cogchar.render.rendtest.GoodyTestMsgMaker
import org.friendularity.navui.TestNavUI.{info1, info0}
import org.friendularity.qpc.{OffersVWorldClient, MakesVWTARqProducers, MakesVWPubStatConsumers, QPidFeatureEndpoint, ExoPubStatDumpingListenerMaker, JmsDestMgr, WritesJmsHeaders, KnowsVWTARqDestinations}
import org.friendularity.thact.ThingActSender
import org.friendularity.vwmsg.{PartialTransform3D, VWTAMsgMaker}

/**
  * Created by Stub22 on 8/11/2016.
  */
trait VWTASender extends ThingActSender with KnowsVWTARqDestinations {
	override def getJmsDestMgr : JmsDestMgr = null
}

// Java friendly class
class VWTASenderTurtleQpid(myJmsSess : javax.jms.Session, jmsHdrWrtr_orNull : WritesJmsHeaders)
			extends VWTASender

import scala.collection.JavaConverters._
class TestTAQPidClient(qpidDestMgr : JmsDestMgr) extends QPidFeatureEndpoint(qpidDestMgr)
			with MakesVWTARqProducers with MakesVWPubStatConsumers with  ExoPubStatDumpingListenerMaker {

	val statDumpPeriod : Int = 5
	myConsumer_forVWPubStatBin.setMessageListener(makePubStatDumpingListener(statDumpPeriod))

	def sendVWRqThingAct(taSpec : ThingActionSpec, encodePref : Integer): Unit = {
		myGenSender.postThingAct(taSpec, encodePref)
	}
}
trait DummyGoodySender extends VarargsLogging {
	def sendSomeVWRqs(testClient : TestTAQPidClient, delayMsec : Int) : Unit = {
		val gtmm: GoodyTestMsgMaker = new GoodyTestMsgMaker
		val msgsJList = gtmm.makeGoodyCreationMsgs
		var msgCount = 0
		val msgSList : List[ThingActionSpec] = msgsJList.asScala.toList
		for (msg <- msgSList) {
			val preferredEncoding : Int = msgCount % 3 // Cycles through 0=no-pref, 1=prefer-bin-ser, 2=prefer-turtle-txt
			if (msg.getTargetThingTypeID.equals(GoodyNames.TYPE_BIT_BOX) && msg.getVerbID.equals(GoodyNames.ACTION_SET)) {
				warn1("Skipping bitBox-set which is reliably crashing the scene-graph when submitted this way, msg={}", msg)
			} else {
				testClient.sendVWRqThingAct(msg, preferredEncoding)
				msgCount += 1
			}
			Thread.sleep(delayMsec)
		}
	}
}
class ClientTestMsgSender(initDelayMsec : Int, stepDelayMsec : Int, sinbadMoves : Boolean, xtraCam : Boolean,
						  goodyPile : Boolean) extends OffersVWorldClient with VWTAMsgMaker  {
	override val myPreferredEncoding : Int = myClient.ENCODE_PREF_BIN

	def initClientConn(): Unit = {
		val clientOffer = this
		info0("========== ClientTestMsgSender.initClientConn() starting CLIENT qpidConn")
		clientOffer.startQpidConn
		clientOffer.checkClient
	}

	def sendMsgs() : Unit = {
		val clientOffer = this
		info1("Client test send thread is sleeping for {} msec", initDelayMsec: Integer)
		Thread.sleep(initDelayMsec)
		//					info0("Client test thread has awoken, sending TA tst messages")
		//					phonyClientOffer.sendTestMsgs
		if (sinbadMoves) {
			val tgtPos = new Vector3f(-20.0f, 90.0f, -20.0f)
			val tgtScl = new Vector3f(12.0f, 3.0f, 8.0f)
			val mxf = new PartialTransform3D(Some(tgtPos), None, Some(tgtScl))
			clientOffer.sendSinbadSmooveRq(mxf, 4.0f)
			Thread.sleep(stepDelayMsec)
		}
		if (xtraCam) {
			val xtraCamGuideShapeID = clientOffer.makeStampyRandyIdent("xtraCam")
			clientOffer.sendRq_makeExtraCamera(xtraCamGuideShapeID)
			Thread.sleep(stepDelayMsec)
			val nextTgtCamPos = new Vector3f(-80.0f, 50.0f, -72.7f)
			val cxf = new PartialTransform3D(Some(nextTgtCamPos), None, None)
			clientOffer.sendRq_moveCamera(xtraCamGuideShapeID, cxf, 20.0f)
			Thread.sleep(stepDelayMsec)
		} else {
			Thread.sleep(stepDelayMsec)
		}

		if (goodyPile) {
			val dummyGoodySender = new DummyGoodySender {}
			dummyGoodySender.sendSomeVWRqs(myClient, stepDelayMsec)
		}

		if (sinbadMoves) {
			val nxtChrPos = new Vector3f(40.0f, 10.0f, 20.0f)
			val nxf = new PartialTransform3D(Some(nxtChrPos), None, None)
			clientOffer.sendSinbadSmooveRq(nxf, 8.0f)
		}
	}
	def startTestThread () {
		initClientConn()
		val testSendThrd = new Thread() {
			override def run: Unit = {
				sendMsgs()
			}
		}
		testSendThrd.start()
	}

}
object RunClientTestMsgSender {
	def main(args: Array[String]): Unit = {

		val (doSinbadMoves, doExtraCam, doGoodyPile) = (true, true, true)

		val clientTestSender = new ClientTestMsgSender(3000, 2000, doSinbadMoves, doExtraCam, doGoodyPile)
		clientTestSender.startTestThread
	}
}