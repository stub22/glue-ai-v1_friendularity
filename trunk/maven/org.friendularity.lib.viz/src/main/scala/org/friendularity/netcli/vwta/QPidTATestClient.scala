package org.friendularity.netcli.vwta

/**
  * Created by Stub22 on 9/5/2016.
  */

import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.api.thing.ThingActionSpec
import org.cogchar.name.goody.GoodyNames
import org.cogchar.render.rendtest.GoodyTestMsgMaker
import org.friendularity.netcli.bigtest.ExoPubStatDumpingListenerMaker
import org.friendularity.qpc.{KnowsVWTARqDestinations, QPidFeatureEndpoint, JmsDestMgr}
import javax.jms.{Destination => JMSDestination, Message => JMSMsg, MessageConsumer => JMSMsgConsumer,
MessageListener => JMSMsgListener, MessageProducer => JMSMsgProducer, ObjectMessage => JMSObjMsg,
Session => JMSSession, TextMessage => JMSTextMsg}


import org.friendularity.thact.{TAEncodePrefs, ThingActSender}


class QPidTATestClient(qpidDestMgr : JmsDestMgr) extends QPidFeatureEndpoint(qpidDestMgr)
			with MakesVWTARqProducers with MakesVWPubStatConsumers with  ExoPubStatDumpingListenerMaker {

	val statDumpPeriod : Int = 5
	myConsumer_forVWPubStatBin.setMessageListener(makePubStatDumpingListener(statDumpPeriod))

	def sendVWRqThingAct(taSpec : ThingActionSpec, encodePref : Integer): Unit = {
		myGenSender.postThingAct(taSpec, encodePref)
	}
}
