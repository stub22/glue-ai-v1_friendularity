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

package org.friendularity.qpc

import java.io.{Serializable => JSerializable}
import java.lang.{Long => JLong}
import javax.jms.{Destination => JMSDestination, Message => JMSMsg, MessageConsumer => JMSMsgConsumer,
		MessageListener => JMSMsgListener, MessageProducer => JMSMsgProducer, ObjectMessage => JMSObjMsg,
		Session => JMSSession, TextMessage => JMSTextMsg}

import org.friendularity.thact.ThingActSender


/** Defines all the VWorld AMQP destination names and JMSDestinations.
  * Created by Stub22 on 8/11/2016.
  */

object VWorldAmqpDestNames {
	// One combined queue destination provides for a single received-request ordering across
	// all clients and both formats.  So we are moving this way in new code.
	val queueName_forUnifiedTA = "vwRqTA_unified" // accepts both serBin and turtleTxt messages

	val topicName_forVWPubStatJSerBin = "vwPubStat_jserBin"

	// However, separate queues for separate formats can occasionally be useful in testing,
	// so we also have these two names avail.
	val queueName_forJSerBinTA = "vwRqTA_jserBin"
	val queueName_forTurtleTxtTA = "vwRqTA_turtleTxt"

	// Used during JNDI-style init, which we don't usually need
	val allDestinNames : List[String] = List(queueName_forUnifiedTA, queueName_forJSerBinTA, queueName_forTurtleTxtTA, topicName_forVWPubStatJSerBin)
}

// This trait can be used on both server and client sides
trait KnowsVWTARqDestinations extends KnowsDestMgr  {
	lazy val destVWRqTATxt : JMSDestination = getDestMgr.makeQueueDestination(VWorldAmqpDestNames.queueName_forTurtleTxtTA)
	lazy val destVWRqTABin : JMSDestination = getDestMgr.makeQueueDestination(VWorldAmqpDestNames.queueName_forJSerBinTA)
	lazy val destVWRqTAUni : JMSDestination = getDestMgr.makeQueueDestination(VWorldAmqpDestNames.queueName_forUnifiedTA)

}
trait KnowsVWPubStatDestinations extends KnowsDestMgr  {
	lazy val destForVWPubStatsBin : JMSDestination = getDestMgr.getDestForTopicName(VWorldAmqpDestNames.topicName_forVWPubStatJSerBin)
}
// TODO:  Add Knows...Producers intf
trait MakesVWTARqProducers extends KnowsVWTARqDestinations {
	private lazy val myJmsSession = getDestMgr.getJmsSession

	def getFlagUnified : Boolean = true
	val myProdForUni : JMSMsgProducer = myJmsSession.createProducer(destVWRqTAUni)

	val myProdForTurtle : JMSMsgProducer = if (getFlagUnified) myProdForUni else myJmsSession.createProducer(destVWRqTATxt)
	val myProdForJSer : JMSMsgProducer = if (getFlagUnified) myProdForUni else myJmsSession.createProducer(destVWRqTABin)

	val myGenSender : ThingActSender = new ThingActSenderQPid(myJmsSession, myProdForJSer, None, myProdForTurtle, None)
}

trait MakesVWPubStatConsumers extends KnowsVWPubStatDestinations {
	private lazy val myJmsSession = getDestMgr.getJmsSession

	lazy val myConsumer_forVWPubStatBin : JMSMsgConsumer = myJmsSession.createConsumer(destForVWPubStatsBin)

}

