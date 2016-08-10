package org.friendularity.qpc

import java.util.Properties
import javax.jms.{Session => JMSSession, Destination => JMSDestination, Connection => JMSConnection, ConnectionFactory => JMSConnectionFactory}
import javax.naming.InitialContext

import org.appdapter.fancy.log.VarargsLogging
import org.jflux.impl.messaging.rk.utils.ConnectionManager

/**
  * Created by Stub22 on 8/8/2016.
  */

trait QpidConnMgr extends VarargsLogging {
	def makeSessionAutoAck() : JMSSession = {
		// Creates a session. This session is not transactional (transactions='false'),
		// and messages are automatically acknowledged.
		// QPid HelloWorld uses AUTO_ACKNOWLEDGE,  // R25 uses   CLIENT_ACKNOWLEDGE
		val conn = getConn
		info2("================= Creating JMS Session in AUTO_ACK mode, for connMgr={}, conn={}", this, conn)
		conn.createSession(false, JMSSession.AUTO_ACKNOWLEDGE);
	}
	def makeFullySpecifiedDestination(destNameTailFull : String) : JMSDestination

	def getConn : JMSConnection
}
class QpidConnMgrJndi(val myJndiProps : Properties) extends QpidConnMgr with VarargsLogging  {
	// The supplied jndiProps are used to define the available destinations, so this current impl
	// does not support dynamically adding topics after the QPidConnector is created.

	val myJndiCtx = new InitialContext(myJndiProps);

	// val myJmsConn : javax.jms.Connection = makeConn_010();
	private val myJmsConn : JMSConnection = makeConn_032();

	private def makeConn_010() : JMSConnection = {
		info0("================= Creating InitialContext")

		val connFactoryKeyTail = QPid_010_Names.qpConnFactoryKey_tail
		info1("================= Looking up ConnFactory at key_tail: {}", connFactoryKeyTail)
		val jmsConnFactory = myJndiCtx.lookup(connFactoryKeyTail).asInstanceOf[JMSConnectionFactory];
		info0("================= Creating Connection")
		val jmsConn  = jmsConnFactory.createConnection();
		jmsConn
	}
	private def makeConn_032() : JMSConnection = {
		info0("================= Creating InitialContext")

		val connFactoryKeyTail = QPid_032_Names.qpConnFactoryKey_tail
		info1("================= Looking up ConnFactory at key_tail: {}", connFactoryKeyTail)
		val jmsConnFactory = myJndiCtx.lookup(connFactoryKeyTail).asInstanceOf[JMSConnectionFactory];
		info0("================= Creating Connection")
		val jmsConn  = jmsConnFactory.createConnection();
		jmsConn
	}

	override def makeFullySpecifiedDestination(destNameTailFull : String) : JMSDestination = {
		// Creates a destination for the topic exchange, so senders and receivers can use it.
		info1("================= Creating Destination for nameTail={}", destNameTailFull)
		// val fullName = QPid_032_Names.destKeyNameForTail(destNameTail)
		// jndiCtx wants just the name tail, rather than the "full name" we specified in jndiProps key (with prefix "destination.").
		val dest : JMSDestination = myJndiCtx.lookup(destNameTailFull).asInstanceOf[JMSDestination];
		info3("Resolved nameTailFull={} to destination clz={}, dump={}", destNameTailFull, dest.getClass, dest)
		dest
	}
	override def getConn : JMSConnection = {
		myJmsConn
	}
	def startConn() : Unit = {
		info1("================= Starting Connection : {}", myJmsConn)
		myJmsConn.start();
	}

	def close() : Unit  = {
		info0("================= Closing JMS Connection : {}")
		myJmsConn.close();
		info0("================= Closing JNDI context")
		myJndiCtx.close();
	}
}