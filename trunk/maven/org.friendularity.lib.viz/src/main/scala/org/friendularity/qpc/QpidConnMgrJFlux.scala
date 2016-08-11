package org.friendularity.qpc

import javax.jms.{Session => JMSSession, Destination => JMSDestination, Connection => JMSConnection, ConnectionFactory => JMSConnectionFactory}


import org.appdapter.fancy.log.VarargsLogging
import org.friendularity.vwimpl.IdentHlp
import org.jflux.impl.messaging.rk.utils.{ConnectionManager => JfluxQpidConnMgr}


/**
  * Created by Stub22  on 8/5/2016.
  */
class QpidConnMgrJFlux extends QpidConnMgr with IdentHlp with VarargsLogging {

	private lazy val myJmsConn : JMSConnection = makeOldeDfltLocalTestQpidConn

	private def makeOldeDfltLocalTestQpidConn: JMSConnection = {
		var qpidConn: JMSConnection = null
		val qpidUsr: String = "admin"
		val qpidPsw: String = "admin"
		val qpidCliName: String = makeStampyRandyString("qpidCli_", "")
		val qpidVHostName: String = "test" // Qpid's default vhost name thru v0.26 was "test"
		val qpidTcpUrl: String = "tcp://127.0.0.1:5672"
		try {
			qpidConn = JfluxQpidConnMgr.createConnection(qpidUsr, qpidPsw, qpidCliName, qpidVHostName, qpidTcpUrl)
		}
		catch {
			case t: Throwable => {
				error1("makeOldeDfltLocalTestQpidConn " + qpidCliName + " caught exception: {}", t)
			}
		}
		return qpidConn
	}
	override def getConn : JMSConnection = myJmsConn
	/*  Copied and modified from JFlux ConnectionUtils
		private static String buildNameString(String destName, int type) */


	override def makeFullySpecifiedDestination(destNameTailFull : String) : JMSDestination = {
		info1("Making destination with full address: {}", destNameTailFull);
		val dest : JMSDestination = JfluxQpidConnMgr.createDestination(destNameTailFull);
		dest

	}
}
