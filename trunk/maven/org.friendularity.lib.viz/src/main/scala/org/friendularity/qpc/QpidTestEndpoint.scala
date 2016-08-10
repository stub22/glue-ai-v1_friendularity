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

trait KnowsTopicConn extends KnowsJmsSession {
	def getTopicConn : QpidDestMgr
}
class QPidTestEndpoint(myQpidTopicMgr : QpidDestMgr) extends KnowsJmsSession with KnowsTopicConn{
	lazy val myJmsSession = myQpidTopicMgr.makeSession

	override protected def getJmsSession : JMSSession = myJmsSession

	override def getTopicConn : QpidDestMgr = myQpidTopicMgr

}

trait ServerFeatureAccess {

	def getVWPubNoticeSender : VWNoticeSender

	def setSerBinListenTeller(tellerLikesSerBin : CPStrongTeller[VWGoodyRqActionSpec])

	def setTurtleTxtListenTeller(tellerLikesSerBin : CPStrongTeller[VWGoodyRqRdf])

	// Should be unnecessary, unless we decide to process more in RDF space with inference+onto.
	// def setTurtleTxtListenTeller (tellerLikesGoodyRdf : CPStrongTeller[VWGoodyRqRdf])

}
trait KnowsOurDestinations extends KnowsTopicConn {
	lazy val destForVWRqTATxt : JMSDestination = getTopicConn.makeQueueDestination(TestAppNames.topicName_forTurtleTxtTA)
	lazy val destForVWRqTABin : JMSDestination = getTopicConn.makeQueueDestination(TestAppNames.topicName_forJSerBinTA)

	lazy val destForVWPubStatsBin : JMSDestination = getTopicConn.getDestForTopicName(TestAppNames.topicName_forVWPubStatJSerBin)

}
trait ServerReceiveFeatureImpl extends ServerFeatureAccess with  KnowsOurDestinations {

	// TODO: These consumers should be for queues (not topics) and in fact often just for a single queue.
	private lazy val myConsumer_forTurtleTxt : JMSMsgConsumer = getJmsSession.createConsumer(destForVWRqTATxt)
	private lazy val myConsumer_forJSerBin : JMSMsgConsumer = getJmsSession.createConsumer(destForVWRqTABin)

	override def setSerBinListenTeller(tellerLikesSerBin : CPStrongTeller[VWGoodyRqActionSpec]) : Unit = {
		val taRcvrBin = new ThingActReceiverBinary(tellerLikesSerBin)
		val listener : JMSMsgListener = taRcvrBin.makeListener
		myConsumer_forJSerBin.setMessageListener(listener)
	}

	override def setTurtleTxtListenTeller(tellerLikesTrtlTxt : CPStrongTeller[VWGoodyRqRdf]) : Unit = {
		val taRcvrTxt : ThingActReceiverTxt = new ThingActReceiverTxt(tellerLikesTrtlTxt)
		val listener : JMSMsgListener = taRcvrTxt.makeListener
		myConsumer_forTurtleTxt.setMessageListener(listener)
	}

}
trait ServerPublishFeatureImpl extends ServerFeatureAccess with  KnowsOurDestinations {
	private val myJmsProdForVWPubNoticeBin : JMSMsgProducer = getJmsSession.createProducer(destForVWPubStatsBin)
	private val mySenderForVWPubNoticeBin :  VWNoticeSender = new VWNoticeSenderJmsImpl(getJmsSession, myJmsProdForVWPubNoticeBin)

	override def getVWPubNoticeSender : VWNoticeSender = mySenderForVWPubNoticeBin
	//	def sendNotice(notice : VWorldNotice): Unit = mySender.postThingAct(taSpec, encodePref)


	def sendSomeVWNotices_Blocking(numNotices : Int, sleepIntervMsec : Int) : Unit = {
		var msgCount = 0
		while (msgCount < numNotices) {
			msgCount += 1
			mySenderForVWPubNoticeBin.sendPingNotice("number = " + msgCount)
			Thread.sleep(sleepIntervMsec)
		}

	}

}

// Receives vw-requests in any ta-format, and publishes vw-notices as binary only.
class TestTAQpidServer(myParentARF : ActorRefFactory, myQpidTopicMgr : QpidDestMgr)
			extends QPidTestEndpoint(myQpidTopicMgr) with ServerFeatureAccess
						with ServerReceiveFeatureImpl with ServerPublishFeatureImpl with DummyActorMaker {


	// Dummy actor+teller to receive thingActions, of both kinds.
	// TODO:  The text-to-binary route can be built in.
	private val dummyRcvActor = makeTestDummyActor(myParentARF, "dummy-goody-rq-rcvr")
	private val dummyRcvWeakTeller = new ActorRefCPMsgTeller(dummyRcvActor)
	// Make extractor wrappers, ask them to make listeners, and attach those listeners to our JmsConsumers
	setSerBinListenTeller(dummyRcvWeakTeller.asInstanceOf[CPStrongTeller[VWGoodyRqActionSpec]])
	setTurtleTxtListenTeller(dummyRcvWeakTeller.asInstanceOf[CPStrongTeller[VWGoodyRqRdf]])

	//	val rcvrFactory = new RecvrFactory
	//	val allPurposeListener = rcvrFactory.makeItWeakerButEasier(rcvWeakTeller)

	// Receiver wrappers know how to extract contents from JmsMessages
	// private  val rcvrTxt : ThingActReceiverTxt = new ThingActReceiverTxt(rcvWeakTeller.asInstanceOf[CPStrongTeller[VWGoodyRqRdf]])
	//private val rcvrBin : ThingActReceiverBinary = new ThingActReceiverBinary(rcvWeakTeller.asInstanceOf[CPStrongTeller[VWGoodyRqActionSpec]])
	//	myConsumer_forTurtleTxt.setMessageListener(rcvrTxt.makeListener)
	//	myConsumer_forJSerBin.setMessageListener(rcvrBin.makeListener)

	// ----------------------------------------------------------------
}
import scala.collection.JavaConverters._
class TestTAQPidClient(ppidTopicMgr : QpidDestMgr)
			extends QPidTestEndpoint(ppidTopicMgr) with KnowsOurDestinations with ExoPubStatDumpingListenerMaker {

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

	lazy val qpidConnMgr : QpidConnMgr = new QpidConnMgrJFlux
	lazy val qpidTopicMgr : QpidDestMgr = new QPidDestMgrJFlux(qpidConnMgr)

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

		val server = new TestTAQpidServer(getAkkaSys, qpidTopicMgr)
		server
	}

	private lazy val myTestClient = {
		val client = new TestTAQPidClient(qpidTopicMgr)
		client
	}

	def getVWPubNoticeSender : VWNoticeSender = myServer.getVWPubNoticeSender

	def getServerFeatureAccess : ServerFeatureAccess = myServer

	def pingQpidSvcs(includeDummyClient : Boolean) : Unit = {
		val noticeSender = myServer.getVWPubNoticeSender
		info2("My Qpid server: {}, noticeSender={}", myServer, noticeSender)
		if (includeDummyClient) {
			info1("My Qpid dummy client: {}", myTestClient)
		}

		noticeSender.sendPingNotice("NavUiApp testing VWNotice send")
	}
}