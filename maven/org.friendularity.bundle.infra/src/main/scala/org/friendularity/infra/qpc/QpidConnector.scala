package org.friendularity.infra.qpc

import javax.jms.{Session => JMSSession, Destination => JMSDestination, Connection => JMSConnection, ConnectionFactory => JMSConnectionFactory}

import org.appdapter.fancy.log.VarargsLogging

/**
  * Created by Stub22 on 8/8/2016.
  */

trait QpidConnMgr extends KnowsJmsConnection with VarargsLogging {
	def makeSessionAutoAck() : JMSSession = {
		// Creates a session. This session is not transactional (transactions='false'),
		// and messages are automatically acknowledged.
		// QPid HelloWorld uses AUTO_ACKNOWLEDGE,  // R25 uses   CLIENT_ACKNOWLEDGE
		val conn = getJmsConnection
		info2("================= Creating JMS Session in AUTO_ACK mode, for connMgr={}, conn={}", this, conn)
		conn.createSession(false, JMSSession.AUTO_ACKNOWLEDGE);
	}
	def makeFullySpecifiedDestination(destNameTailFull : String) : JMSDestination

	override def getJmsConnection : JMSConnection

	def startConn: Unit = {
		val qpidConn = getJmsConnection
		info1("Now starting qpidConn, so any existing consumers should already have listeners set up.  conn={}", qpidConn)
		qpidConn.start
	}
}


/*
https://docs.oracle.com/javaee/7/api/javax/jms/Connection.html

"A JMS client typically creates a connection, one or more sessions, and a number of message producers and consumers.
When a connection is created, it is in stopped mode. That means that no messages are being delivered.

It is typical to leave the connection in stopped mode until setup is complete (that is, until all message consumers
have been created). At that point, the client calls the connection's start method, and messages begin arriving at
the connection's consumers. This setup convention minimizes any client confusion that may result from asynchronous
message delivery while the client is still in the process of setting itself up.

A connection can be started immediately, and the setup can be done afterwards. Clients that do this must be prepared
to handle asynchronous message delivery while they are still in the process of setting up.

A message producer can send messages while a connection is stopped."

https://docs.oracle.com/javaee/7/api/javax/jms/Session.html

"A session can create and service multiple message producers and consumers.

One typical use is to have a thread block on a synchronous MessageConsumer until a message arrives. The thread may
then use one or more of the Session's MessageProducers.

If a client desires to have one thread produce messages while others consume them, the client should use a
separate session for its producing thread.

Once a connection has been started, any session with one or more registered message listeners is dedicated to the
thread of control that delivers messages to it. It is erroneous for client code to use this session or any of its
constituent objects from another thread of control. The only exception to this rule is the use of the session or
message consumer close method."

 */