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

import org.friendularity.respire.VarargsLogging

/**
 * @author Stu B. <www.texpedient.com>
 * 
 * Uses the flowy(->snazzy) ontology types to configure agents that read/write to/from both graphs and topics.
 * 
 * We expect that when graphs are written, a quad-store monitor can pick up the changes and publish them to
 * a set of topics.  The granularity of publish is determined using two factors:
 *		1) granule of graphStore transaction
 *		2) granule of topic subscription
 *		
 *	The intersection of these defines the granule of an event payload = {deletes + inserts}, which is tagged
 *	with metadata indicating (via receipts) what transaction generated the event.  So, for example
 *	
 *	agent-A1 says to quadStore-Q1 { operation-UI-Action {subtype KeyPress (or webPush, or 
 */

object TopicalRouterTest extends VarargsLogging {
	def main(args: Array[String]) : Unit = {
		// Backup - if logging is not working, try enabling these two lines.
		// Must enable "compile" or "provided" scope for Log4J dep in order to compile this code.
		// Note that these settings can cause double-logging, if there is a log4j.properties found.
		org.apache.log4j.BasicConfigurator.configure();
		org.apache.log4j.Logger.getRootLogger().setLevel(org.apache.log4j.Level.ALL);
		info0("Let's have a tropical discussion.")
		val qhw = new QpidHelloWorld()
		qhw.chopWood()
		info0("This conversation is FINISHED.")
	}
}
import javax.jms.{ConnectionFactory, Session, Destination}
import javax.jms.{MessageConsumer, MessageProducer}
import javax.jms.{Message, BytesMessage, TextMessage}
import javax.jms.JMSException;

import javax.naming.Context;
import javax.naming.InitialContext;
import java.util.Properties;
class QpidHelloWorld extends VarargsLogging {
	def chopWood() = {
		try {
			val jndiProps = new Properties();
			// properties.load(this.getClass().getResourceAsStream("hello.properties"));
			// These values are taken from the QPid 0.10 "hello.properties"
			// Later versions of QPid use different values.
			jndiProps.put("java.naming.factory.initial", "org.apache.qpid.jndi.PropertiesFileInitialContextFactory")
			// connectionfactory.[jndiname] = [ConnectionURL]
			jndiProps.put("connectionfactory.qpidConnectionfactory", "amqp://guest:guest@clientid/test?brokerlist='tcp://localhost:5672'")
			// Register an AMQP destination in JNDI
			// destination.[jniName] = [Address Format]
			jndiProps.put("destination.topicExchange", "amq.topic")
			info0("================= Creating InitialContext")
			val jndiContext = new InitialContext(jndiProps);
			info0("================= Looking up qpidConnectionFactory")
			val jmsConnFactory = jndiContext.lookup("qpidConnectionfactory").asInstanceOf[ConnectionFactory];
			info0("================= Creating Connection")
			val jmsConn  = jmsConnFactory.createConnection();
			info0("================= Starting Connection")
			jmsConn.start();	  
			// Creates a session. This session is not transactional (transactions='false'), 
			// and messages are automatically acknowledged.
			info0("================= Creating Session")
			
		
			// QPid HelloWorld uses AUTO_ACKNOWLEDGE,  // R25 uses   CLIENT_ACKNOWLEDGE
			val jmsSession = jmsConn.createSession(false, Session.AUTO_ACKNOWLEDGE); 
			// Creates a destination for the topic exchange, so senders and receivers can use it.
			info0("================= Creating Destination")
			val jmsDestination = jndiContext.lookup("topicExchange").asInstanceOf[Destination];
			info0("================= Creating Producer")
			val jmsMsgProducer = jmsSession.createProducer(jmsDestination);
			info0("================= Creating Consumer")
			val jmsMsgConsumer = jmsSession.createConsumer(jmsDestination);

			info0("*********************\n********************* Calling send-and-consume")
			sendAndConsumeTestMsg(jmsSession, jmsMsgProducer, jmsMsgConsumer)
			info0("*********************\n********************* Calling send-and-consume")
			sendAndConsumeTestMsg(jmsSession, jmsMsgProducer, jmsMsgConsumer)
			// TODO:  Put the close calls into a finally block
			info0("================= Closing JMS Connection")
			jmsConn.close();
			info0("================= Closing JNDI context")
			jndiContext.close();
		} catch  {
			case except : Exception => {
				except.printStackTrace();
			}
		}
	}
	def sendAndConsumeTestMsg(jmsSession : Session, jmsMsgProducer : MessageProducer, jmsMsgConsumer : MessageConsumer ) { 
		info0("================= Creating Text Message")
		val jmsTxtMsg_toSend = jmsSession.createTextMessage("Big Phat MASSAGE!");

		val preSendSysTimeMsec = System.currentTimeMillis
		val preSendMsgID = jmsTxtMsg_toSend.getJMSMessageID
		val preSendStampNone = jmsTxtMsg_toSend.getJMSTimestamp
		// On the *first* message sent, there can be is a longish (perhaps 2-400 msec) delay (due to classloading and
		// other setup, no doubt) between the preSendSysTimeMsec and the eventual postSendStamp below.
		// On subsequent messages it is a smaller delay.
		info3("================= Sending Text Message (at t={}), messageID={} probably NULL, sentStamp={} probably zero", 
				preSendSysTimeMsec : java.lang.Long, preSendMsgID, preSendStampNone : java.lang.Long)
		jmsMsgProducer.send(jmsTxtMsg_toSend);
		val postSendMsgID = jmsTxtMsg_toSend.getJMSMessageID
		val postSendStamp : Long = jmsTxtMsg_toSend.getJMSTimestamp() // This is the java-time that the message was sent.
		val postSendSysTimeMsec = System.currentTimeMillis
		info3("================= Message was sent, the ID is now={} and Timestamp={}, now={}", postSendMsgID, 
				postSendStamp : java.lang.Long, postSendSysTimeMsec : java.lang.Long)

		info0("================= Receiving Text Message")
		val rcvdMsg =  jmsMsgConsumer.receive().asInstanceOf[TextMessage];
		val rcvdMsgID : String = rcvdMsg.getJMSMessageID()			
		val rcvdMsgTstamp : Long = rcvdMsg.getJMSTimestamp() // This is the java-time that the message was sent.
		val rcvdText : String = rcvdMsg.getText()
		info3("Received text message, ID={}, Timestamp={}, text={}", rcvdMsgID, rcvdMsgTstamp : java.lang.Long, rcvdText);		
	}
}	


/*
 *  public String getJMSMessageID() throws JMSException
 *  
http://docs.oracle.com/javaee/1.4/api/javax/jms/Message.html
 *
Gets the message ID.
The JMSMessageID header field contains a value that uniquely identifies each message sent by a provider.

When a message is sent, JMSMessageID can be ignored. When the send or publish method returns, it contains a 
provider-assigned value.

A JMSMessageID is a String value that should function as a unique key for identifying messages in a historical 
repository. The exact scope of uniqueness is provider-defined. It should at least cover all messages for a specific 
installation of a provider, where an installation is some connected set of message routers.

All JMSMessageID values must start with the prefix 'ID:'. Uniqueness of message ID values across different providers 
is not required.

Since message IDs take some effort to create and increase a message's size, some JMS providers may be able to 
optimize message overhead if they are given a hint that the message ID is not used by an application. By calling 
the MessageProducer.setDisableMessageID method, a JMS client enables this potential optimization for all messages 
sent by that message producer. If the JMS provider accepts this hint, these messages must have the message ID set 
to null; if the provider ignores the hint, the message ID must be set to its normal unique value.
==================================================================================================================
*/
/*
			 * public long getJMSTimestamp()
                     throws JMSException
Gets the message timestamp.
The JMSTimestamp header field contains the time a message was handed off to a provider to be sent. 
It is not the time the message was actually transmitted, because the actual send may occur later due to 
transactions or other client-side queueing of messages.

When a message is sent, JMSTimestamp is ignored. When the send or publish method returns, it contains a time value 
somewhere in the interval between the call and the return. The value is in the format of a normal millis time value 
in the Java programming language.

Since timestamps take some effort to create and increase a message's size, some JMS providers may be able to optimize 
message overhead if they are given a hint that the timestamp is not used by an application. By calling the 
MessageProducer.setDisableMessageTimestamp method, a JMS client enables this potential optimization for all messages 
sent by that message producer. If the JMS provider accepts this hint, these messages must have the timestamp set to 
zero; if the provider ignores the hint, the timestamp must be set to its normal value.

 * R25/JFlux is using:
 * 
 * ConnectionUtils
 *             session = con.createSession(false, Session.CLIENT_ACKNOWLEDGE);

 
        if(type == QUEUE){
            fullName += "; {create: always, node: {type: queue}}";
        }else if(type == TOPIC){
            fullName += "; {create: always, node: {type: topic}}";
        }

For a topic exchange, if no subject is specified and no x-bindings are defined for the link, the subscription queue 
is bound using a wildcard matching any routing key (thus satisfying the expectation that any message sent to that 
address will be received from it). If a subject is specified in the source address however, it is used for the binding 
key (this means that the subject in the source address may be a binding pattern including wildcards).

 http://qpid.apache.org/releases/qpid-0.14/books/Programming-In-Apache-Qpid/html/ch02s04.html

*/