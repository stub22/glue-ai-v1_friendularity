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
import org.friendularity.vwmsg.{VWGoodyRqActionSpec, VWGoodyRqRdf, VWorldNotice}

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

class QPidTestEndpoint(myQpidTopicMgr : QpidTopicConn) {
	lazy val myJmsSession = myQpidTopicMgr.makeSession

	lazy val destForVWRqTATxt : JMSDestination = myQpidTopicMgr.getDestForTopicName(TestAppNames.topicName_forTurtleTxtTA)
	lazy val destForVWRqTABin : JMSDestination = myQpidTopicMgr.getDestForTopicName(TestAppNames.topicName_forJSerBinTA)

	lazy val destForVWPubStatsBin : JMSDestination = myQpidTopicMgr.getDestForTopicName(TestAppNames.topicName_forVWPubStatJSerBin)
}

// Receives vw-requests in any ta-format, and publishes vw-notices as binary only.
class TestTAQpidServer(myParentARF : ActorRefFactory, myQpidTopicMgr : QpidTopicConn)
			extends QPidTestEndpoint(myQpidTopicMgr) with DummyActorMaker {

	lazy val myConsumer_forTurtleTxt : JMSMsgConsumer = myJmsSession.createConsumer(destForVWRqTATxt)
	lazy val myConsumer_forJSerBin : JMSMsgConsumer = myJmsSession.createConsumer(destForVWRqTABin)

	val rcvActor = makeTestDummyActor(myParentARF, "dummy-goody-rq-rcvr")
	val rcvWeakTeller = new ActorRefCPMsgTeller(rcvActor)

	//	val rcvrFactory = new RecvrFactory
	//	val allPurposeListener = rcvrFactory.makeItWeakerButEasier(rcvWeakTeller)

	val rcvrTxt : ThingActReceiverTxt = new ThingActReceiverTxt(rcvWeakTeller.asInstanceOf[CPStrongTeller[VWGoodyRqRdf]])
	val rcvrBin : ThingActReceiverBinary = new ThingActReceiverBinary(rcvWeakTeller.asInstanceOf[CPStrongTeller[VWGoodyRqActionSpec]])

	myConsumer_forTurtleTxt.setMessageListener(rcvrTxt.makeListener)
	myConsumer_forJSerBin.setMessageListener(rcvrBin.makeListener)

	val myJmsProdForVWPubNoticeBin : JMSMsgProducer = myJmsSession.createProducer(destForVWPubStatsBin)
	val mySenderForVWPubNoticeBin :  VWNoticeSender = new VWNoticeSenderJmsImpl(myJmsSession, myJmsProdForVWPubNoticeBin)

	//	def sendNotice(notice : VWorldNotice): Unit = mySender.postThingAct(taSpec, encodePref)

	def sendPingNotice(txt : String): Unit = {
		val pingNotice = new FunVWNoticeImpl("VW Pub Stat Ping Notice: " + txt)
		mySenderForVWPubNoticeBin.sendVWNotice(pingNotice)

	}
	def sendSomeVWNotices : Unit = {
		var msgCount = 0
		while (msgCount < 50) {
			sendPingNotice("number = " + msgCount)
			msgCount += 1
			Thread.sleep(900)
		}

	}
}
import scala.collection.JavaConverters._
class TestTAQPidClient(ppidTopicMgr : QpidTopicConn)
			extends QPidTestEndpoint(ppidTopicMgr) with VWPubStatListenerMaker {

	val myProdForTurtle : JMSMsgProducer = myJmsSession.createProducer(destForVWRqTATxt)
	val myProdForJSer : JMSMsgProducer = myJmsSession.createProducer(destForVWRqTABin)

	val mySender = new ThingActSenderQPid(myJmsSession, myProdForJSer, None, myProdForTurtle, None)
	lazy val myConsumer_forVWPubStatBin : JMSMsgConsumer = myJmsSession.createConsumer(destForVWPubStatsBin)

	myConsumer_forVWPubStatBin.setMessageListener(makePubStatNoticeListener)

	def sendVWRqThingAct(taSpec : ThingActionSpec, encodePref : Integer): Unit = mySender.postThingAct(taSpec, encodePref)

	def sendSomeVWRqs : Unit = {
		val gtmm: GoodyTestMsgMaker = new GoodyTestMsgMaker
		val msgsJList = gtmm.makeGoodyCreationMsgs
		var msgCount = 0
		val msgSList : List[ThingActionSpec] = msgsJList.asScala.toList
		for (msg <- msgSList) {
			sendVWRqThingAct(msg, msgCount % 3)
			msgCount += 1
			Thread.sleep(1200)
		}
	}
}

trait OffersQpidSvcs extends KnowsAkkaSys with VarargsLogging {
	// lazy val qpidTopicMgr : QpidTopicConn = new QPidTopicConn_JNDI_032(TestAppNames.allTopics)

	lazy val qpidConnMgr : QpidConnMgr = new QpidConnMgrJFlux
	lazy val qpidTopicMgr : QpidTopicConn = new QPidTopicConnJFlux(qpidConnMgr)


	lazy val myServer = {
		val server = new TestTAQpidServer(getAkkaSys, qpidTopicMgr)
		server
	}
	lazy val myTestClient = {
		val client = new TestTAQPidClient(qpidTopicMgr)
		client
	}
	def pingQpidSvcs : Unit = {
		info1("My Qpid server: {}", myServer)
		info1("My Qpid test client: {}", myTestClient)
		myServer.sendPingNotice("NavUiApp testing VWNotice send")
	}
}