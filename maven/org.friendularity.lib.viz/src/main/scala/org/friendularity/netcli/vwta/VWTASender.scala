package org.friendularity.netcli.vwta

import javax.jms.Session

import org.friendularity.qpc.{QpidDestMgr, WritesJmsHeaders, KnowsTARqDestinations}

/**
  * Created by Stub22 on 8/11/2016.
  */
trait VWTASender extends KnowsTARqDestinations {
	override def getDestMgr : QpidDestMgr = null
}

// Java friendly class
class VWTASenderTurtleQpid(myJmsSess : javax.jms.Session, jmsHdrWrtr_orNull : WritesJmsHeaders)
			extends VWTASender {

}
