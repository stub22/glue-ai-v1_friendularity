package org.friendularity.qpc

import javax.jms.{Session => JMSSession, Connection => JMSConnection, Destination => JMSDestination, MessageConsumer => JMSMsgConsumer, MessageListener => JMSMsgListener, MessageProducer => JMSMsgProducer}

/**
  * Created by Stub22 on 8/11/2016.
  */

trait KnowsJmsSession {
	def getJmsSession : JMSSession
}

trait KnowsDestMgr {
	def getJmsDestMgr : JmsDestMgr
}

trait KnowsJmsConnection {
	def getJmsConnection : JMSConnection
}
