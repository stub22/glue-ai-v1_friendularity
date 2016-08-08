package org.friendularity.qpc

import javax.jms.{Session => JMSSession, Destination => JMSDestination, Connection => JMSConnection, ConnectionFactory => JMSConnectionFactory}


import org.appdapter.fancy.log.VarargsLogging
import org.jflux.impl.messaging.rk.utils.{ConnectionManager => JfluxQpidConnMgr}


/**
  * Created by Stub22  on 8/5/2016.
  */
class QpidConnMgrJFlux extends QpidConnMgr with VarargsLogging {

	private lazy val myJmsConn : JMSConnection = makeOldeDfltLocalTestQpidConn

	private def makeOldeDfltLocalTestQpidConn: JMSConnection = {
		var qpidConn: JMSConnection = null
		val qpidUsr: String = "admin"
		val qpidPsw: String = "admin"
		val qpidCliName: String = "client1"
		val qpidVHostName: String = "test"
		val qpidTcpUrl: String = "tcp://127.0.0.1:5672"
		try {
			qpidConn = JfluxQpidConnMgr.createConnection(qpidUsr, qpidPsw, qpidCliName, qpidVHostName, qpidTcpUrl)
			qpidConn.start
		}
		catch {
			case t: Throwable => {
				error1("makeOldeDfltLocalTestQpidConn caught exception: {}", t)
			}
		}
		return qpidConn
	}
	override def getConn : JMSConnection = myJmsConn
	/*  Copied and modified from JFlux ConnectionUtils
		private static String buildNameString(String destName, int type) */

	val QUEUE_PROPS_SUFFIX = "; {create: always, node: {type: queue}}";
	val TOPIC_PROPS_SUFFIX = "; {create: always, node: {type: topic}}";

	private def makeTopicDestination(destNameTail: String): JMSDestination = {
		val destStr = destNameTail + TOPIC_PROPS_SUFFIX
		val dest : JMSDestination = JfluxQpidConnMgr.createDestination(destStr);
		dest
	}

	override def makeDestination(destNameTail : String) : JMSDestination = {
		makeTopicDestination(destNameTail)
	}
}
