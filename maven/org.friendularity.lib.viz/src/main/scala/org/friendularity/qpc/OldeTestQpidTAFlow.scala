/*
 *  Copyright 2014 by The Friendularity Project (www.friendularity.org).
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

/**
 * @author Stu B. <www.texpedient.com>
 */

import org.appdapter.core.name.{FreeIdent, Ident}
import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.api.vworld.GoodyActionParamWriter
import org.cogchar.impl.thing.basic.{BasicThingActionSpec, BasicTypedValueMap, BasicTypedValueMapWithConversion}
import org.cogchar.name.goody.GoodyNames
import org.friendularity.thact.DummyThingActionMaker

/**
  * OLDE 2014 prototype test
  *
 * Demonstration of ThingAction sent as a serialized Java object through JMS interface to QPid.
 * This approach is interoperable with other Java agents, but not with general non-Java AMQP agents.
 * For platform-independent sharing of ThingActions, general agents can use HTTP-SPARQL mechanisms, but those
 * do not presently provide async notification.   Hence our nascent effort to put a proper RDF-to-(avro/)AMQP
 * serializer+publisher into Fuseski-based semantic server deployment.  See discussion on "Jena-Users" 
 * mailing list.
 * 
 * Meanwhile, this java-serialized ThingAction payload allows us to complete initial implementations of
 * Glue.AI features, which will eventually migrate to use the proper serializations over AMQP, when those 
 * are available.   (Possible incremental step:  Offer AMQP-published string-serializations of RDF-turtle, 
 * within some restrictions on payload).
 */
import javax.jms.{MapMessage, MessageConsumer, MessageProducer, ObjectMessage, Session};

class OldeTestQpidTAFlow extends DummyThingActionMaker {
	def sendAndConsumeTAMsg(jmsSession : Session, jmsMsgProducer : MessageProducer, jmsMsgConsumer : MessageConsumer ) {
		info0("================= Creating ObjectMessage to hold TA")

		val thingActionKey = "thingAction"
		val objMsg_toSend : ObjectMessage = jmsSession.createObjectMessage()

		val mapMsg_toSend : MapMessage = jmsSession.createMapMessage()

		objMsg_toSend.setIntProperty("wackyInt", 987654321);
		objMsg_toSend.setStringProperty("wackyName", "Widget");
		objMsg_toSend.setDoubleProperty("wackyPrice", 0.99);
		val thingActionSpec = makeThingActionSpec
		// We cannot nest arbitrary serializable objects into MapMessages, apparently.
		// In QPid 0.10, we get: javax.jms.MessageFormatException: Cannot set property thingAction to value BasicThingActionSpec[]
		// 	at org.apache.qpid.client.message.AMQPEncodedMapMessage.setObject(AMQPEncodedMapMessage.java:75)
		// mapMsg_toSend.setObject(thingActionKey, thingActionSpec)
		objMsg_toSend.setObject(thingActionSpec)
		// mapMsg_toSend.setObject("ta1", thingActionSpec)

		val msgToSend = objMsg_toSend

		val preSendSysTimeMsec = System.currentTimeMillis
		val preSendMsgID = msgToSend.getJMSMessageID
		val preSendStampNone = msgToSend.getJMSTimestamp
		// On the *first* message sent, there can be is a longish (perhaps 2-400 msec) delay (due to classloading and
		// other setup, no doubt) between the preSendSysTimeMsec and the eventual postSendStamp below.
		// On subsequent messages it is a smaller delay.
		info3("================= Sending BTAS Obj-Message (at t={}), messageID={} probably NULL, pre-sentStamp={} probably zero",
				preSendSysTimeMsec : java.lang.Long, preSendMsgID, preSendStampNone : java.lang.Long)
		jmsMsgProducer.send(msgToSend);
		val postSendMsgID = msgToSend.getJMSMessageID
		val postSendStamp : Long = msgToSend.getJMSTimestamp() // This is the java-time that the message was sent.
		val postSendSysTimeMsec = System.currentTimeMillis
		info3("================= Message was sent,  ID={} post-sent-stamp={}, now={}", postSendMsgID,
				postSendStamp : java.lang.Long, postSendSysTimeMsec : java.lang.Long)

		info0("================= Receiving BTAS-ObjMessage")
		val objMsg_rcvd =  jmsMsgConsumer.receive().asInstanceOf[ObjectMessage];
		// val mapMsg_rcvd =  jmsMsgConsumer.receive().asInstanceOf[MapMessage];
		val msgRcvd = objMsg_rcvd
		val rcvdMsgID : String = msgRcvd.getJMSMessageID()
		val rcvdMsgTstamp : Long = msgRcvd.getJMSTimestamp() // This is the java-time that the message was sent.

		info3("Received objMessage, ID={}, Timestamp={}, msg={}", rcvdMsgID, rcvdMsgTstamp : java.lang.Long, msgRcvd);
		val rcvdBTAS : BasicThingActionSpec =  objMsg_rcvd.getObject().asInstanceOf[BasicThingActionSpec]
		info1("Extracted BTAS: {}", rcvdBTAS)

	}

}
