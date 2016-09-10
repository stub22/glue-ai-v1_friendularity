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
import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.api.thing.ThingActionSpec
import org.cogchar.name.goody.GoodyNames
import org.cogchar.render.rendtest.GoodyTestMsgMaker
import org.friendularity.navui.TestNavUI.{info1, info0}
import org.friendularity.qpc.{QPidFeatureEndpoint, JmsDestMgr, WritesJmsHeaders, KnowsVWTARqDestinations}
import org.friendularity.thact.{TAEncodePrefs, ThingActSender}
import org.friendularity.vwmsg.{PartialTransform3D}

import javax.jms.{Destination => JMSDestination, Message => JMSMsg,
		MessageConsumer => JMSMsgConsumer, MessageListener => JMSMsgListener,
		MessageProducer => JMSMsgProducer, ObjectMessage => JMSObjMsg,
		Session => JMSSession, TextMessage => JMSTextMsg}

/**
  * Created by Stub22 on 8/11/2016.
  */
/*
trait VWTASender extends ThingActSender with KnowsVWTARqDestinations {
	override def getJmsDestMgr : JmsDestMgr = null
}

// Java friendly class
class VWTASenderTurtleQpid(myJmsSess : javax.jms.Session, jmsHdrWrtr_orNull : WritesJmsHeaders)
			extends VWTASender
*/

import scala.collection.JavaConverters._
// TODO:  Add Knows...Producers intf
trait MakesVWTARqProducers extends KnowsVWTARqDestinations with TAEncodePrefs {
	private lazy val myJmsSession = getJmsDestMgr.getJmsSession

	def getFlagUnified : Boolean = true

	private lazy val myProdForUni : JMSMsgProducer = myJmsSession.createProducer(destVWRqTAUni)

	private lazy val myProdForTurtle : JMSMsgProducer = if (getFlagUnified) myProdForUni else myJmsSession.createProducer(destVWRqTATxt)
	private lazy val myProdForJSer : JMSMsgProducer = if (getFlagUnified) myProdForUni else myJmsSession.createProducer(destVWRqTABin)
	protected val myGenSender : ThingActSender = new ThingActSenderQPid(myJmsSession, myProdForJSer, None, myProdForTurtle, None)

	val ENCODE_NO_PREF = NO_PREFERENCE
	val ENCODE_PREF_BIN = PREFER_JAVA_SER
	val ENCODE_PREF_TRT = PREFER_TURTLE_SER
}


trait DummyGoodySender extends VarargsLogging {
	def sendSomeVWRqs(testClient : QPidTATestClient, delayMsec : Int) : Unit = {
		val gtmm: GoodyTestMsgMaker = new GoodyTestMsgMaker
		val msgsJList = gtmm.makeGoodyCreationMsgs
		var msgCount = 0
		val msgSList : List[ThingActionSpec] = msgsJList.asScala.toList
		for (msg <- msgSList) {
			val preferredEncoding : Int = msgCount % 3 // Cycles through 0=no-pref, 1=prefer-bin-ser, 2=prefer-turtle-txt
			if (msg.getTargetThingTypeID.equals(GoodyNames.TYPE_BIT_BOX)
						&& msg.getVerbID.equals(GoodyNames.ACTION_SET)) {
				warn1("Skipping bitBox-set which is reliably crashing the scene-graph when submitted this way, msg={}", msg)
			} else {
				testClient.sendVWRqThingAct(msg, preferredEncoding)
				msgCount += 1
			}
			Thread.sleep(delayMsec)
		}
	}
}
