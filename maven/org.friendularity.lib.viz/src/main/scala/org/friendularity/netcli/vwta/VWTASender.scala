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

import org.cogchar.api.thing.ThingActionSpec
import org.cogchar.render.rendtest.GoodyTestMsgMaker
import org.friendularity.qpc.{MakesVWTARqProducers, MakesVWPubStatConsumers, QPidFeatureEndpoint, ExoPubStatDumpingListenerMaker, QpidDestMgr, WritesJmsHeaders, KnowsVWTARqDestinations}

/**
  * Created by Stub22 on 8/11/2016.
  */
trait VWTASender extends KnowsVWTARqDestinations {
	override def getDestMgr : QpidDestMgr = null
}

// Java friendly class
class VWTASenderTurtleQpid(myJmsSess : javax.jms.Session, jmsHdrWrtr_orNull : WritesJmsHeaders)
			extends VWTASender {

}

import scala.collection.JavaConverters._
class TestTAQPidClient(qpidDestMgr : QpidDestMgr) extends QPidFeatureEndpoint(qpidDestMgr)
			with MakesVWTARqProducers with MakesVWPubStatConsumers with  ExoPubStatDumpingListenerMaker {

	val statDumpPeriod : Int = 5
	myConsumer_forVWPubStatBin.setMessageListener(makePubStatDumpingListener(statDumpPeriod))

	def sendVWRqThingAct(taSpec : ThingActionSpec, encodePref : Integer): Unit = {
		myGenSender.postThingAct(taSpec, encodePref)
	}

	def sendSomeVWRqs(delayMsec : Int) : Unit = {
		val gtmm: GoodyTestMsgMaker = new GoodyTestMsgMaker
		val msgsJList = gtmm.makeGoodyCreationMsgs
		var msgCount = 0
		val msgSList : List[ThingActionSpec] = msgsJList.asScala.toList
		for (msg <- msgSList) {
			val preferredEncoding : Int = msgCount % 3 // Cycles through 0=no-pref, 1=prefer-bin-ser, 2=prefer-turtle-txt
			sendVWRqThingAct(msg, preferredEncoding)
			msgCount += 1
			Thread.sleep(delayMsec)
		}
	}
}