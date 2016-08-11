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

import java.io.Serializable

import java.io.{Serializable => JSerializable}
import java.lang.{Long => JLong}
import javax.jms.{Destination => JMSDestination, Message => JMSMsg, MessageConsumer => JMSMsgConsumer,
		MessageListener => JMSMsgListener, MessageProducer => JMSMsgProducer, ObjectMessage => JMSObjMsg,
		Session => JMSSession, TextMessage => JMSTextMsg}

import org.appdapter.fancy.log.VarargsLogging

/**
  * Created by Owner on 8/8/2016.
  */
trait WritesJmsHeaders {
	def putHeadersOnMsg(msg : JMSMsg, containedData : Any): Unit
}
trait JmsMsgSenderChan extends KnowsJmsSession with VarargsLogging {
	def sendCompleteMsg(jmsProd : JMSMsgProducer, completeMsgHasHeadersAlready : JMSMsg): Unit = {
		val destJustForLogging = jmsProd.getDestination
		// Earlier we had an error causing the destination to be too general, displaying as:  'amq.topic'/'#'; None
		debug2("JmsMsgSenderChan destination clz={}, dump={}", destJustForLogging.getClass, destJustForLogging)
		jmsProd.send(completeMsgHasHeadersAlready)

	}
	def sendJmsObjMsgPlusHeaders(jmsProd : JMSMsgProducer, objToSend : JSerializable, headerWriter_opt : Option[WritesJmsHeaders]): Unit = {
		val jmsSession = getJmsSession
		val objMsg_toSend : JMSObjMsg = jmsSession.createObjectMessage
		objMsg_toSend.setObject(objToSend)
		headerWriter_opt.map(_.putHeadersOnMsg(objMsg_toSend, objToSend))
		sendCompleteMsg(jmsProd, objMsg_toSend)
	}
	def sendJmsTxtMsgPlusHeaders(jmsProd : JMSMsgProducer, txtToSend : String, headerWriter_opt : Option[WritesJmsHeaders]): Unit = {
		val jmsSession = getJmsSession
		val txtMsg_toSend : JMSTextMsg = jmsSession.createTextMessage(txtToSend)
		headerWriter_opt.map(_.putHeadersOnMsg(txtMsg_toSend, txtToSend))
		sendCompleteMsg(jmsProd, txtMsg_toSend)
	}

}

trait DefinedJmsSenderChan extends JmsMsgSenderChan {
	def getJmsProducer : JMSMsgProducer
	def getHeaderWriter_opt : Option[WritesJmsHeaders]

	def sendJmsObjMsg(objToSend : JSerializable): Unit = {
		sendJmsObjMsgPlusHeaders(getJmsProducer, objToSend, getHeaderWriter_opt)
	}
	def sendJmsTxtMsg(txtToSend : String) : Unit = {
		sendJmsTxtMsgPlusHeaders(getJmsProducer, txtToSend, getHeaderWriter_opt)
	}
}
class JmsSenderChanImpl(myJmsSess : JMSSession, myJmsProd: JMSMsgProducer, myHdWrtrOpt : Option[WritesJmsHeaders])
			extends  DefinedJmsSenderChan  with KnowsJmsSession {
	override def getJmsSession : JMSSession = myJmsSess

	override def getJmsProducer : JMSMsgProducer = myJmsProd
	override def getHeaderWriter_opt : Option[WritesJmsHeaders] = myHdWrtrOpt

}
