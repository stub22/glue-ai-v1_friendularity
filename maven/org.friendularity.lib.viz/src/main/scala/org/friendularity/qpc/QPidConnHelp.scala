package org.friendularity.qpc

import javax.jms.Connection

import org.appdapter.fancy.log.VarargsLogging
import org.jflux.impl.messaging.rk.utils.ConnectionManager

/**
  * Created by Owner on 8/5/2016.
  */
class QPidConnHelp extends VarargsLogging {

	def makeOldeDfltLocalTestQpidConn: Connection = {
		var qpidConn: Connection = null
		val qpidUsr: String = "admin"
		val qpidPsw: String = "admin"
		val qpidCliName: String = "client1"
		val qpidVHostName: String = "test"
		val qpidTcpUrl: String = "tcp://127.0.0.1:5672"
		try {
			qpidConn = ConnectionManager.createConnection(qpidUsr, qpidPsw, qpidCliName, qpidVHostName, qpidTcpUrl)
			qpidConn.start
		}
		catch {
			case t: Throwable => {
				error1("makeOldeDfltLocalTestQpidConn caught exception: {}", t)
			}
		}
		return qpidConn
	}
}
