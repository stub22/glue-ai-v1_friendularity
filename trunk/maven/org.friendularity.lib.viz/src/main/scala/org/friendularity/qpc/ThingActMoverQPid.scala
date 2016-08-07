/*
 *  Copyright 2016 by The Friendularity Project (www.friendularity.org).
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package org.friendularity.qpc

import java.io.{Serializable => JSerializable}
import java.lang.{Long => JLong}
import javax.jms.{Destination => JMSDestination, Message => JMSMsg, MessageConsumer => JMSMsgConsumer,
		MessageListener => JMSMsgListener, MessageProducer => JMSMsgProducer, ObjectMessage => JMSObjMsg,
		Session => JMSSession, TextMessage => JMSTextMsg}

import akka.actor.{Actor, ActorRef, ActorRefFactory, ActorSystem, Props}
import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.api.thing.ThingActionSpec
import org.cogchar.render.rendtest.GoodyTestMsgMaker
import org.friendularity.akact.DummyActorMaker
import org.friendularity.cpmsg.{ActorRefCPMsgTeller, CPStrongTeller}

import org.friendularity.thact.{ThingActReceiverBinary, ThingActReceiverTxt, ThingActSender, ThingActTurtleEncoder}
import org.friendularity.vwmsg.{VWorldNotice, VWGoodyRqActionSpec, VWGoodyRqRdf, VWGoodyRqTAS, VWGoodyRqTurtle}

/**
  * Created by StuB22 on 6/13/2016.
  *
  * We are able to move, with equivalent meaning, either/both:
  * 1) Strings, containing RDF-turtle serialized TASpec
  * 2) Java-serialized binary TASpec
  *
  * Each of these kinds may flow over QPid *or* akka.
  * Thus we say the choice of transport (akka, QPid, HTTP, ...) is
  * orthogonal to choice of message serialization (RDF-turtle, java serial, JSON(-LD), avro, RDF-thrift, ...)
  *
  * Each of these pathways has a send-side and a receive-side.
  *
  * In order to do responses, as request-reply or as stream of notices,
  * additional assumptions must be made above/outside this Mover level.
  * Akka makes async responses of any kind easy in a Java client,
  * as long as we accept peer-to-peer to configurable ports on each side.
  * (Again, question of string vs. binary payload remains orthogonal).
  */

trait KnowsJmsSession {
	protected def getJmsSession : JMSSession
}
trait WritesJmsHeaders {
	def putHeadersOnMsg(msg : JMSMsg, containedData : Any): Unit
}
trait JmsMsgSenderChan extends KnowsJmsSession with VarargsLogging {
	def sendCompleteMsg(jmsProd : JMSMsgProducer, completeMsgHasHeadersAlready : JMSMsg): Unit = {
		val destJustForLogging = jmsProd.getDestination
		// Earlier we had an error causing the destination to be too general, displaying as:  'amq.topic'/'#'; None
		info2("JmsMsgSenderChan destination clz={}, dump={}", destJustForLogging.getClass, destJustForLogging)
		jmsProd.send(completeMsgHasHeadersAlready)

	}
	/*
	def sendMsgPlusHeaders(jmsProd : JMSMsgProducer, msgStillNeedsHeaders : JMSMsg, contentToSend : AnyRef,
						   headerWriter_opt : Option[WritesJmsHeaders]) : Unit = {
		headerWriter_opt.map(_.putHeadersOnMsg(objMsg_toSend, objToSend))
	}*/
	def sendJmsObjMsgPlusHeaders(jmsProd : JMSMsgProducer, objToSend : JSerializable, headerWriter_opt : Option[WritesJmsHeaders]): Unit = {
		val jmsSession = getJmsSession
		val objMsg_toSend : JMSObjMsg = jmsSession.createObjectMessage
		objMsg_toSend.setObject(objToSend)
		headerWriter_opt.map(_.putHeadersOnMsg(objMsg_toSend, objToSend))
		sendCompleteMsg(jmsProd, objMsg_toSend)
	}
	def sendJmsTxtMsgPlusHeaders(jmsProd : JMSMsgProducer, txtToSend : String, headerWriter_opt : Option[WritesJmsHeaders]): Unit = {
		val jmsSession = getJmsSession
		val txtMsg_toSend : JMSTextMsg = jmsSession.createTextMessage(txtToSend)
		headerWriter_opt.map(_.putHeadersOnMsg(txtMsg_toSend, txtToSend))
		sendCompleteMsg(jmsProd, txtMsg_toSend)
		/*
		val dest = jmsProd.getDestination
		info2("ThingAct-TurtleTxt destination clz={}, dump={}", dest.getClass, dest)
		jmsProd.send(txtMsg_toSend)
		*/
	}

}

trait DefinedJmsSenderChan extends JmsMsgSenderChan {
	def getJmsProducer : JMSMsgProducer
	def getHeaderWriter_opt : Option[WritesJmsHeaders]

	def sendJmsObjMsg(objToSend : JSerializable): Unit = {
		sendJmsObjMsgPlusHeaders(getJmsProducer, objToSend, getHeaderWriter_opt)
	}
	def sendJmsTxtMsg(txtToSend : String) : Unit = {
		sendJmsTxtMsgPlusHeaders(getJmsProducer, txtToSend, getHeaderWriter_opt)
	}
}
class JmsSenderChanImpl(myJmsSess : JMSSession, myJmsProd: JMSMsgProducer, myHdWrtrOpt : Option[WritesJmsHeaders])
		extends  DefinedJmsSenderChan  with KnowsJmsSession {
	override def getJmsSession : JMSSession = myJmsSess

	override def getJmsProducer : JMSMsgProducer = myJmsProd
	override def getHeaderWriter_opt : Option[WritesJmsHeaders] = myHdWrtrOpt

}

//trait TASenderQPid extends VarargsLogging {
	//def getDefinedSendChan_TATurtle : DefinedJmsSenderChan
	//def getDefinedSendChan_TAJSer : DefinedJmsSenderChan
	/*
	protected def getJmsProducer_TATurtle : JMSMsgProducer
	protected def getJmsProducer_TAJSer : JMSMsgProducer
*/
//	def sendTAJSerMsg(objToSend : JSerializable): Unit = {
//		val sender = getDefinedSendChan_TAJSer
//		sender.sendJmsObjMsg(objToSend)
		/*
		val jmsProd = getJmsProducer_TAJSer
		sendJmsObjMsg(jmsProd, objToSend)
		val jmsSession = getJmsSession
		val objMsg_toSend : JMSObjMsg = jmsSession.createObjectMessage
		objMsg_toSend.setObject(objToSend)
		putHeadersOnMsg(objMsg_toSend)
		val dest = jmsProd.getDestination
		// Earlier we had an error causing the destination to be too general, displaying as:  'amq.topic'/'#'; None
		info2("ThingAct-JavaSer destination clz={}, dump={}", dest.getClass, dest)
		jmsProd.send(objMsg_toSend)
		*/
//	}

//	def sendTxtMsg(txtToSend : String) : Unit = {
//		val sender = getDefinedSendChan_TATurtle
//		sender.sendJmsTxtMsg(txtToSend)
		/*
		val jmsSession = getJmsSession
		val txtMsg_toSend : JMSTextMsg = jmsSession.createTextMessage(strToSend)
		putHeadersOnMsg(txtMsg_toSend)
		val jmsProd = getJmsProducer_TATurtle
		val dest = jmsProd.getDestination
		info2("ThingAct-TurtleTxt destination clz={}, dump={}", dest.getClass, dest)
		jmsProd.send(txtMsg_toSend)
		*/
//	}
//
//}

class ThingActSenderQPid(myJmsSess : JMSSession, myJmsProd_JSer : JMSMsgProducer, serHdrWrtr : Option[WritesJmsHeaders],
						 myJmsProd_Turtle : JMSMsgProducer, trtHdrWrtr : Option[WritesJmsHeaders])
					extends ThingActSender  with KnowsJmsSession {

	override protected def getJmsSession : JMSSession = myJmsSess

	lazy val mySendChan_TAJSer : DefinedJmsSenderChan = new JmsSenderChanImpl(myJmsSess, myJmsProd_JSer, serHdrWrtr)
	lazy val mySendChan_TATurtle : DefinedJmsSenderChan = new JmsSenderChanImpl(myJmsSess, myJmsProd_Turtle, trtHdrWrtr)

	// lavy val mySendChan
	// override protected def getJmsProducer_TAJSer : JMSMsgProducer = myJmsProd_JSer
	// override protected def getJmsProducer_TATurtle : JMSMsgProducer = myJmsProd_Turtle

	lazy val myTurtleEncoder = new ThingActTurtleEncoder{}

	override def supportsJavaSer : Boolean = true
	override def supportsTurtleSer : Boolean = true

	override def postThingActViaJavaSer(srlzblTASpec : ThingActionSpec): Unit = {
		info1("Sending java-ser TA msg for action={}", srlzblTASpec.getActionSpecID)
		mySendChan_TAJSer.sendJmsObjMsg(srlzblTASpec.asInstanceOf[JSerializable])
		// sendJavaSerMsg(taSpec.asInstanceOf[JSerializable])
	}
	override def postThingActViaTurtleSer(taSpec : ThingActionSpec): Unit = {
		info1("Sending turtle-txt TA msg for action={}", taSpec.getActionSpecID)
		val taAsTurtleTxt : String  = myTurtleEncoder.encodeAsTurtleMsg(taSpec)
		mySendChan_TATurtle.sendJmsTxtMsg(taAsTurtleTxt)
		// sendTxtMsg(taAsTurtleTxt)
	}

}
class DummyTestHeaderWriter() extends WritesJmsHeaders {
	override def putHeadersOnMsg(msg : JMSMsg, dataInside : Any): Unit = {
		msg.setIntProperty("wackyInt", 987654321);
		msg.setStringProperty("wackyName", "Widget");
		msg.setDoubleProperty("wackyPrice", 0.99);
	}
}

trait VWNoticeSender {
	def sendVWNotice(vwNotice : VWorldNotice)
}
class VWNoticeSenderJmsImpl(myJmsSess : JMSSession, myJmsProd_JSer : JMSMsgProducer) extends VWNoticeSender with KnowsJmsSession {
	override protected def getJmsSession : JMSSession = myJmsSess
	val myHdrWriter : Option[WritesJmsHeaders] = None
	lazy val myNoticeSendChan : DefinedJmsSenderChan = new JmsSenderChanImpl(myJmsSess, myJmsProd_JSer, myHdrWriter)
	override def sendVWNotice(vwNotice : VWorldNotice) : Unit = {
		myNoticeSendChan.sendJmsObjMsg(vwNotice)
	}
}


class QPidTopicConn_032(myTopicExchangeNameList : List[String]) extends VarargsLogging {
	lazy val myNameMgr = new QPid_032_NameManager()

	lazy val myJndiProps = myNameMgr.makeJndiPropsForTopicSetup(myTopicExchangeNameList)

	lazy val myQPidConn : QPidConnector = {
		info1("QPidConn jndiProps={}", myJndiProps)
		val qc = new QPidConnector(myJndiProps)
		qc.startConn()
		qc
	}

	def makeSession : JMSSession = myQPidConn.makeSessionAutoAck()

	lazy val myDestsByNameTail : Map[String, JMSDestination] = myTopicExchangeNameList.map(n => n -> myQPidConn.makeDestination(n)).toMap

}
object TestAppNames {
	// Nothing stops us from making these names the same, combining destinations, etc, if that's what we wanted.
	// But separate queues for separate formats is a decent approach.
	val topicName_forJSerBinTA = "vwRqTA_jserBin"
	val topicName_forTurtleTxtTA = "vwRqTA_turtleTxt"

	val topicName_forVWPubStatJSerBin = "vwPubStat_jserBin"

	val allTopics = List(topicName_forJSerBinTA, topicName_forTurtleTxtTA, topicName_forVWPubStatJSerBin)

}

class QPidTestEndpoint(myQPidConnMgr : QPidTopicConn_032) {
	lazy val myJmsSession = myQPidConnMgr.makeSession

	lazy val destForVWRqTATxt : JMSDestination = myQPidConnMgr.myDestsByNameTail.get(TestAppNames.topicName_forTurtleTxtTA).get
	lazy val destForVWRqTABin : JMSDestination = myQPidConnMgr.myDestsByNameTail.get(TestAppNames.topicName_forJSerBinTA).get

	lazy val destForVWPubStatsBin : JMSDestination = myQPidConnMgr.myDestsByNameTail.get(TestAppNames.topicName_forVWPubStatJSerBin).get
}

case class FunVWNoticeImpl(funTxt : String) extends VWorldNotice

class TestTAQpidServer(myParentARF : ActorRefFactory, qpidConnMgr : QPidTopicConn_032)
			extends QPidTestEndpoint(qpidConnMgr) with DummyActorMaker {

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

	def sendSomeVWNotices : Unit = {
		var msgCount = 0
		while (msgCount < 60) {
			val someNotice = new FunVWNoticeImpl("VW Pub Stat notice number = " + msgCount)
			mySenderForVWPubNoticeBin.sendVWNotice(someNotice)
			msgCount += 1
			Thread.sleep(900)
		}

	}

}
trait ClientStatListenerCouldUseAkkaButThatWouldBeWeird extends  DummyActorMaker {
	val myParentARF : ActorRefFactory = ???
	val wwStatRcvActor = makeTestDummyActor(myParentARF, "vwStatRcvr")
	val vwStatWeakTeller = new ActorRefCPMsgTeller(wwStatRcvActor)
}

trait VWPubStatListenerMaker extends VarargsLogging {
	def makePubStatNoticeListener : JMSMsgListener = {
		new JMSMsgListener() {
			override def onMessage(msg: JMSMsg): Unit = {
				info2("ThingActReceiverBinary-JMSListener msgID={} timestamp={}", msg.getJMSMessageID, msg.getJMSTimestamp : JLong)
				debug1("ThingActReceiverBinary-JMSListener - received msg, dumping to see if 'wacky' headers show up:\n{}", msg)
				msg match {
					case objMsg: JMSObjMsg => {
						val objCont : JSerializable = objMsg.getObject
						info2("VWPubStatListener received objMsg with tstamp={}, notice={}", objMsg.getJMSTimestamp: JLong, objCont.toString)
	//					receiveJSerBinaryMsg(objMsg)
					}
					case otherMsg => {
						error2("Received unexpected (not JMS-ObjectMessage) message, class={}, dump=\n{}", otherMsg.getClass,  otherMsg)
					}
				}
			}
		}
	}
}
import scala.collection.JavaConverters._
class TestTAQPidClient(myParentARF : ActorRefFactory, qpidConnMgr : QPidTopicConn_032)
			extends QPidTestEndpoint(qpidConnMgr) with VWPubStatListenerMaker {

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
object ThingActMoverQPid_UnitTest extends VarargsLogging {
	val srvrAkkaSysName = "unit-test-ta-qpid-vwSrvr"
	lazy private val myServerAkkaSys = ActorSystem(srvrAkkaSysName)

	// When using akka-remote, this sys would need to have a separate port number.
	// val cliAkkaSysName = "unit-test-ta-qpid-vwCli"
	// lazy private val myCliAkkaSys = ActorSystem(cliAkkaSysName)

	def main(args: Array[String]) : Unit = {
		info1("TAMover test started with cmd-line-args array={}", args)

		val qpidConnMgr = new QPidTopicConn_032(TestAppNames.allTopics)

		info1("QPidConnMgr.DestMap={}", qpidConnMgr.myDestsByNameTail)

		// server and client are in same Java process, same qpid-conn, but separate JMSSessions.
		val server = new TestTAQpidServer(myServerAkkaSys, qpidConnMgr)

		// Uses same akka-sys, otherwise we would need a separate akka-remote port number.
		val client = new TestTAQPidClient(myServerAkkaSys, qpidConnMgr)

		client.sendSomeVWRqs

		server.sendSomeVWNotices
	}

}

