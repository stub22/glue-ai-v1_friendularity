package org.friendularity.vw.impl.ta

import org.friendularity.akact.KnowsAkkaSys
import org.friendularity.cpmsg.CPStrongTeller
import org.friendularity.qpc.{OffersQpidSomething, VWNoticeSender}
import org.friendularity.vw.msg.ta.VWRqTAWrapper

/**
  * Created by Owner on 2/18/2017.
  */
trait OffersVWorldQpidServer extends KnowsAkkaSys with OffersQpidSomething  {
	// lazy val qpidTopicMgr : QpidTopicConn = new QPidTopicConn_JNDI_032(TestAppNames.allTopics)

	// Under OSGi the server and client cannot easily deserialize messages with JMSMessage.getObject
	// (Needs classpath wiring help - would it help to set context-cl during onMessage handler?).
	// However they can *make* serialized messages OK, and send them out for consumption by
	// simpler Java clients1.  Inside OSGi we can also read turtle-text messages, avro messages,
	// JMSText message, JMSMap messages, etc.
	/*
		 [java] 30399   INFO [Dispatcher-1-Conn-1] org.friendularity.qpc.TestTAQPidClient (HasLogger.scala:31) info2 - VWPubStatListener-JMSListener msgID=ID:866e1fa5-e6c2-33f4-9770-4f3e8bffecfd timestamp=1470709600728
		 [java] 30400  ERROR [Dispatcher-1-Conn-1] org.apache.qpid.client.BasicMessageConsumer (BasicMessageConsumer.java:795) notifyMessage - reNotification : Caught exception (dump follows) - ignoring...
		 [java] javax.jms.MessageFormatException: Unable to deserialize message
		 [java] 	at org.apache.qpid.client.message.JMSObjectMessage.getObject(JMSObjectMessage.java:160)
		 [java] 	at org.friendularity.qpc.VWPubStatListenerMaker$$anon$1.onMessage(ThingActMoverQPid.scala:86)
		 [java] 	at org.apache.qpid.client.BasicMessageConsumer.notifyMessage(BasicMessageConsumer.java:777)
	*/
	private lazy val myServer : ServerFeatureAccess = {
		val server = new TestTAQpidServer(getAkkaSys, myQpidConnMgr)
		server
	}

	private def getServerReceiveFeature : ServerReceiveFeature = myServer.getServerRecieveFeature
	private def getServerPublishFeature : ServerPublishFeature = myServer.getServerPublishFeature

	def setUnifiedListenTeller(tellerLikesBin : CPStrongTeller[VWRqTAWrapper]) : Unit = {
		getServerReceiveFeature.setUnifiedListenTeller(tellerLikesBin)
	}

	def getVWPubNoticeSender : VWNoticeSender = getServerPublishFeature.getVWPubNoticeSender


	def checkServerSvcs() : Unit = {
		val noticeSender = getVWPubNoticeSender
		info2("My Qpid server: {}, noticeSender={}", myServer, noticeSender)
		noticeSender.sendPingNotice("NavUiApp testing VWNotice send")
	}
}
