package org.friendularity.qpc

import java.io.{Serializable => JSerializable}
import java.lang.{Long => JLong}
import javax.jms.{Destination => JMSDestination, Message => JMSMsg, MessageConsumer => JMSMsgConsumer,
		MessageListener => JMSMsgListener, MessageProducer => JMSMsgProducer, ObjectMessage => JMSObjMsg,
		Session => JMSSession, TextMessage => JMSTextMsg}


import akka.actor.ActorRefFactory
import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.api.thing.ThingActionSpec
import org.cogchar.render.rendtest.GoodyTestMsgMaker
import org.friendularity.akact.{KnowsAkkaSys, DummyActorMaker}
import org.friendularity.cpmsg.{CPStrongTeller, ActorRefCPMsgTeller}
import org.friendularity.thact.{ThingActReceiverBinary, ThingActReceiverTxt}
import org.friendularity.vwmsg.{VWGoodyRqRdf, VWGoodyRqActionSpec, VWorldNotice}

/**
  * Created by Owner on 8/8/2016.
  */
object TestAppNames {
	// Nothing stops us from making these names the same, combining destinations, etc, if that's what we wanted.
	// But separate queues for separate formats is a decent approach.
	val topicName_forJSerBinTA = "vwRqTA_jserBin"
	val topicName_forTurtleTxtTA = "vwRqTA_turtleTxt"

	val topicName_forVWPubStatJSerBin = "vwPubStat_jserBin"

	val allTopics = List(topicName_forJSerBinTA, topicName_forTurtleTxtTA, topicName_forVWPubStatJSerBin)
}

trait KnowsDestMgr extends KnowsJmsSession {
	def getDestMgr : QpidDestMgr
}
class QPidFeatureEndpoint(myJmsDestMgr : QpidDestMgr) extends KnowsJmsSession with KnowsDestMgr {
	lazy val myJmsSession = myJmsDestMgr.makeSession

	override protected def getJmsSession : JMSSession = myJmsSession

	override def getDestMgr : QpidDestMgr = myJmsDestMgr

}

// This trait can be used on both server and client sides
trait KnowsTARqDestinations extends KnowsDestMgr  {
	lazy val destForVWRqTATxt : JMSDestination = getDestMgr.makeQueueDestination(TestAppNames.topicName_forTurtleTxtTA)
	lazy val destForVWRqTABin : JMSDestination = getDestMgr.makeQueueDestination(TestAppNames.topicName_forJSerBinTA)

}
trait KnowsPubStatDestinations extends KnowsDestMgr  {
	lazy val destForVWPubStatsBin : JMSDestination = getDestMgr.getDestForTopicName(TestAppNames.topicName_forVWPubStatJSerBin)
}

import scala.collection.JavaConverters._
class TestTAQPidClient(qpidDestMgr : QpidDestMgr) extends QPidFeatureEndpoint(qpidDestMgr)
			with KnowsTARqDestinations with KnowsPubStatDestinations with  ExoPubStatDumpingListenerMaker {

	val myProdForTurtle : JMSMsgProducer = myJmsSession.createProducer(destForVWRqTATxt)
	val myProdForJSer : JMSMsgProducer = myJmsSession.createProducer(destForVWRqTABin)

	val mySender = new ThingActSenderQPid(myJmsSession, myProdForJSer, None, myProdForTurtle, None)
	lazy val myConsumer_forVWPubStatBin : JMSMsgConsumer = myJmsSession.createConsumer(destForVWPubStatsBin)

	val statDumpPeriod : Int = 5
	myConsumer_forVWPubStatBin.setMessageListener(makePubStatDumpingListener(statDumpPeriod))

	def sendVWRqThingAct(taSpec : ThingActionSpec, encodePref : Integer): Unit = mySender.postThingAct(taSpec, encodePref)

	def sendSomeVWRqs(delayMsec : Int) : Unit = {
		val gtmm: GoodyTestMsgMaker = new GoodyTestMsgMaker
		val msgsJList = gtmm.makeGoodyCreationMsgs
		var msgCount = 0
		val msgSList : List[ThingActionSpec] = msgsJList.asScala.toList
		for (msg <- msgSList) {
			val preferredEncoding : Int = msgCount % 3 // Cycles through 0=no-pref, 1=prefer-bin-ser, 2=prefer-turtle-txt
			sendVWRqThingAct(msg, preferredEncoding)
			msgCount += 1
			Thread.sleep(delayMsec)
		}
	}
}

trait OffersQpidSvcs extends KnowsAkkaSys with VarargsLogging {
	// lazy val qpidTopicMgr : QpidTopicConn = new QPidTopicConn_JNDI_032(TestAppNames.allTopics)

	private lazy val myQpidConnMgr : QpidConnMgr = new QpidConnMgrJFlux

	var myConnStartedFlag : Boolean = false

	def startQpidConn : Unit = {
		if (!myConnStartedFlag) {
			myQpidConnMgr.startConn
		} else {
			warn1("Qpid connection was already started for {}, ignoring extra request to do so", this)
		}
	}
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

	def getServerReceiveFeature : ServerReceiveFeature = myServer.getServerRecieveFeature

	def getVWPubNoticeSender : VWNoticeSender = myServer.getServerPublishFeature.getVWPubNoticeSender

	private lazy val myTestClient = {
		val clientDestMgr : QpidDestMgr = new QPidDestMgrJFlux(myQpidConnMgr)
		val client = new TestTAQPidClient(clientDestMgr)
		client
	}


	def pingQpidSvcs(includeDummyClient : Boolean) : Unit = {
		val noticeSender = getVWPubNoticeSender
		info2("My Qpid server: {}, noticeSender={}", myServer, noticeSender)
		if (includeDummyClient) {
			info1("My Qpid dummy client: {}", myTestClient)
		}

		noticeSender.sendPingNotice("NavUiApp testing VWNotice send")
	}
}