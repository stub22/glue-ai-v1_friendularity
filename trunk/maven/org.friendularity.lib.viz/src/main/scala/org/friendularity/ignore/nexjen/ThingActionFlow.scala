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

package org.friendularity.ignore.nexjen

/**
 * @author Stu B. <www.texpedient.com>
 */

trait Dummy99

import org.appdapter.fancy.log.VarargsLogging

import org.appdapter.core.name.{Ident, SerIdent, FreeIdent}
import org.cogchar.api.thing.ThingActionSpec
import org.cogchar.impl.thing.basic.{BasicTypedValueMap, BasicTypedValueMapWithConversion}
import org.cogchar.impl.thing.basic.BasicThingActionSpec;


import org.cogchar.api.vworld.GoodyActionParamWriter;
import org.cogchar.name.goody.GoodyNames;


/**
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
class ThingActionFlow extends VarargsLogging {
	val TEST_INIT_X = 30.0f;
	val TEST_INIT_Y = 15.0f;
	val TEST_INIT_Z = 10.0f;
	val goodyGraphQN = "ccrt:thing_sheet_22";
	val boxBaseURI = "http://dummy.org/bitbox#num_";	
	
	def makeThingActionSpec () : BasicThingActionSpec = {
		val btvm : BasicTypedValueMap = new BasicTypedValueMapWithConversion(); // ConcreteTVM();
		val gapw = new GoodyActionParamWriter(btvm);
		
		gapw.putLocation(TEST_INIT_X, TEST_INIT_Y, TEST_INIT_Z);
		gapw.putRotation(1.0f, 1.0f, 1.0f, 10.0f);
		//gapw.putSize(4f, 0f, 0f);
		gapw.putScaleUniform(4f);

		val dummyBoxID = new FreeIdent(boxBaseURI + System.currentTimeMillis());
		// sendBitBoxTAS(dummyBoxID, GoodyNames.ACTION_CREATE, btvm, ran, debugFlag);
		
		// 	public void sendBitBoxTAS(Ident tgtThingID, Ident verbID, SerTypedValueMap paramTVMap, Random ran, boolean debugFlag) {
		// return dummyBoxID;
		// 
import java.util.Random;

		val ran = new Random();
		val actRecID = new FreeIdent("action_#" + ran.nextInt());
		val verbID = GoodyNames.ACTION_CREATE
		val tgtThingID = dummyBoxID
		val tgtThingTypeID = GoodyNames.TYPE_BIT_BOX;
		val paramTVMap = btvm
		val srcAgentID : Ident = null;
		val postedTStampMsec = System.currentTimeMillis();
		val btas = new BasicThingActionSpec(actRecID, tgtThingID, tgtThingTypeID, verbID, srcAgentID, paramTVMap, postedTStampMsec);	
		// sendThingActionSpec(btas, ran, debugFlag);
		btas
	
	}
}

import javax.jms.{ConnectionFactory, Session, Destination}
import javax.jms.{MessageConsumer, MessageProducer}
import javax.jms.{Message, BytesMessage, TextMessage, MapMessage, ObjectMessage}
import javax.jms.JMSException;

import javax.naming.Context;
import javax.naming.InitialContext;
import java.util.Properties;

class TAFlow_QPid extends ThingActionFlow {
		def sendAndConsumeTAMsg(jmsSession : Session, jmsMsgProducer : MessageProducer, jmsMsgConsumer : MessageConsumer ) {
		info0("================= Creating ObjectMessage to hold TA")

		val thingActionKey = "thingAction"
		val objMsg_toSend : ObjectMessage = jmsSession.createObjectMessage()

		val mapMsg_toSend : MapMessage = jmsSession.createMapMessage()

		objMsg_toSend.setIntProperty("wackyInt", 987654321);
		objMsg_toSend.setStringProperty("wackyName", "Widget");
		objMsg_toSend.setDoubleProperty("wackyPrice", 0.99);
		val thingActionSpec = makeThingActionSpec
		// Wecannot nest arbitrary serializable objects into MapMessages, apparently.
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
