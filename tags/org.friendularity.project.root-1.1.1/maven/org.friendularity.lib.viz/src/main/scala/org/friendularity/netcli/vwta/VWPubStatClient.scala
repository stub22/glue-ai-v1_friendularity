package org.friendularity.netcli.vwta

import javax.jms.{MessageConsumer => JMSMsgConsumer}

import org.friendularity.vw.api.amqp.KnowsVWPubStatDestinations

/**
  * Created by Stub22 on 9/5/2016.
  */
trait MakesVWPubStatConsumers extends KnowsVWPubStatDestinations {
	private lazy val myJmsSession = getJmsDestMgr.getJmsSession

	lazy val myConsumer_forVWPubStatBin : JMSMsgConsumer = myJmsSession.createConsumer(destForVWPubStatsBin)

}
