package org.friendularity.qpc

import javax.jms.{Destination => JMSDestination, Message => JMSMsg, MessageConsumer => JMSMsgConsumer,
MessageListener => JMSMsgListener, MessageProducer => JMSMsgProducer, ObjectMessage => JMSObjMsg,
Session => JMSSession, TextMessage => JMSTextMsg}

/**
  * Created by Stub22 on 8/11/2016.
  */

trait KnowsJmsSession {
	def getJmsSession : JMSSession
}

trait KnowsDestMgr { // extends KnowsJmsSession {
	def getDestMgr : QpidDestMgr

	//	override def getJmsSession : JMSSession = getDestMgr.getJmsSession
}

