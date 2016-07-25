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

package org.friendularity.dull

import javax.jms.{Session => JMSSession, Message => JMSMsg, ObjectMessage => JMSObjMsg, TextMessage => JMSTextMsg,
		MessageProducer => JMSMsgProducer, Destination => JMSDestination, MessageConsumer => JMSMsgConsumer,
		MessageListener => JMSMsgListener}

import java.lang.{Long => JLong}
import java.io.{Serializable => JSerializable}

import akka.actor.{ActorSystem, Actor, ActorLogging, Props, ActorRef, ActorRefFactory}
import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.api.thing.ThingActionSpec


import org.cogchar.render.goody.basic.BasicGoodyCtx
import org.cogchar.render.rendtest.GoodyTestMsgMaker
import org.friendularity.cpump.{ActorRefCPMsgTeller, CPMsgTeller, CPStrongTeller}
import org.friendularity.ignore.nexjen.{QPidConnector, QPid_032_NameManager}
import org.friendularity.vwmsg.{VWGoodyRqTurtle, VWGoodyRqRdf, VWGoodyRqTAS, VWGoodyRqActionSpec}


import scala.collection.immutable.HashMap

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


// To be used on client side where they may not have CPump awareness
trait ThingActSender extends VarargsLogging {
	val NO_PREFERENCE : Integer = 0
	val PREFER_JAVA_SER : Integer = 1
	val PREFER_TURTLE_SER : Integer = 2

	def supportsJavaSer : Boolean = false
	def supportsTurtleSer : Boolean = false
	def postThingAct(taSpec : ThingActionSpec, encodePref : Integer): Unit = {
		var sent = false
		if ((encodePref == PREFER_JAVA_SER) || (!supportsTurtleSer)) {
			if (supportsJavaSer) {
				postThingActViaJavaSer(taSpec)
				sent=true
			}
		}
		if (!sent) {
			if (supportsTurtleSer) {
				postThingActViaTurtleSer(taSpec)
				sent=true
			}
		}
		if (!sent) {
			error0("TA-Message not sent, no serialization pathways supported")
		}
	}

	def postThingActViaJavaSer(taSpec : ThingActionSpec): Unit = ???
	def postThingActViaTurtleSer(taSpec : ThingActionSpec): Unit = ???
}

trait SenderQPid extends VarargsLogging {
	protected def getJmsSession : JMSSession
	protected def getJmsProducer_TATurtle : JMSMsgProducer
	protected def getJmsProducer_TAJSer : JMSMsgProducer
	protected def putHeadersOnMsg(msg : JMSMsg): Unit

	def sendJavaSerMsg(objToSend : JSerializable): Unit = {
		val jmsSession = getJmsSession
		val objMsg_toSend : JMSObjMsg = jmsSession.createObjectMessage
		objMsg_toSend.setObject(objToSend)
		putHeadersOnMsg(objMsg_toSend)
		val jmsProd = getJmsProducer_TAJSer
		val dest = jmsProd.getDestination
		// Earlier we had an error causing the destination to be too general, displaying as:  'amq.topic'/'#'; None
		info2("ThingAct-JavaSer destination clz={}, dump={}", dest.getClass, dest)
		jmsProd.send(objMsg_toSend)
	}

	def sendTxtMsg(strToSend : String) : Unit = {
		val jmsSession = getJmsSession
		val txtMsg_toSend : JMSTextMsg = jmsSession.createTextMessage(strToSend)
		putHeadersOnMsg(txtMsg_toSend)
		val jmsProd = getJmsProducer_TATurtle
		val dest = jmsProd.getDestination
		info2("ThingAct-TurtleTxt destination clz={}, dump={}", dest.getClass, dest)
		jmsProd.send(txtMsg_toSend)
	}

}

class ThingActSenderQPid(myJmsSess : JMSSession, myJmsProd_JSer : JMSMsgProducer,
						 myJmsProd_Turtle : JMSMsgProducer)
					extends ThingActSender with SenderQPid {

	lazy val myTurtleEncoder = new ThingActTurtleEncoder{}

	override def supportsJavaSer : Boolean = true
	override def supportsTurtleSer : Boolean = true

	override protected def getJmsSession : JMSSession = myJmsSess
	override protected def getJmsProducer_TAJSer : JMSMsgProducer = myJmsProd_JSer
	override protected def getJmsProducer_TATurtle : JMSMsgProducer = myJmsProd_Turtle

	override def postThingActViaJavaSer(taSpec : ThingActionSpec): Unit = {
		info1("Sending java-ser TA msg for action={}", taSpec.getActionSpecID)
		sendJavaSerMsg(taSpec.asInstanceOf[JSerializable])
	}
	override def postThingActViaTurtleSer(taSpec : ThingActionSpec): Unit = {
		info1("Sending turtle-txt TA msg for action={}", taSpec.getActionSpecID)
		val taAsTurtleTxt : String  = myTurtleEncoder.encodeAsTurtleMsg(taSpec)
		sendTxtMsg(taAsTurtleTxt)
	}
	override protected def putHeadersOnMsg(msg : JMSMsg): Unit = {
		msg.setIntProperty("wackyInt", 987654321);
		msg.setStringProperty("wackyName", "Widget");
		msg.setDoubleProperty("wackyPrice", 0.99);
	}
}
// JMS spec requires Listener's onMessage method to only be called serially [...although if we attach same listener to
// multiple consumers, probably can be called concurrently, eh?].   We don't really need that invariant
// here, cince we just statelessly forward the message to an actor destination.
// Impl Notes:
// This listener sniffs at the inbound JMS message to find type, and dispatches to Receiver code.
// Receiver code wraps and sends an appropriate actor msg to enqueue further work on the message.

class ThingActReceiverTxt(goodyTATurtleTeller : CPStrongTeller[VWGoodyRqRdf]) extends VarargsLogging {
	def receiveTextMsg(txtMsg : JMSTextMsg) : Unit = {
		val txtCont = txtMsg.getText
		// We assume it is a turtle encoding of ThingActSpec(s), targeting VW-Goodies.
		val goodyTATurtleRq = new VWGoodyRqTurtle(txtCont)
		goodyTATurtleTeller.tellStrongCPMsg(goodyTATurtleRq)
	}
	def makeListener : JMSMsgListener = {
		new JMSMsgListener() {
			override def onMessage(msg: JMSMsg): Unit = {
				info2("ThingActReceiverTxt-JMSListener msgID={} timestamp={}", msg.getJMSMessageID, msg.getJMSTimestamp : JLong)
				debug1("ThingActReceiverTxt-JMSListener - received msg, dumping to see if 'wacky' headers show up:\n{}", msg)
				msg match {
					case txtMsg: JMSTextMsg => {
						info1("Listener processing received txtMsg with tstamp={}", txtMsg.getJMSTimestamp: JLong)
						receiveTextMsg(txtMsg)
					}
					case other => {
						error2("Received unexpected  (not JMS-TextMessage) message class={}, dump=\n{}", other.getClass, other)
					}
				}
			}
		}
	}
}
class ThingActReceiverBinary(goodyTADirectTeller : CPStrongTeller[VWGoodyRqActionSpec]) extends VarargsLogging  {
	def receiveJSerBinaryMsg(objMsg : JMSObjMsg) : Unit = {
		val objCont = objMsg.getObject
		val taSpec : ThingActionSpec = objCont.asInstanceOf[ThingActionSpec]
		val goodyTADirectRq = new VWGoodyRqTAS(taSpec)
		goodyTADirectTeller.tellStrongCPMsg(goodyTADirectRq)
	}
	def makeListener : JMSMsgListener = {
		new JMSMsgListener() {
			override def onMessage(msg: JMSMsg): Unit = {
				info2("ThingActReceiverBinary-JMSListener msgID={} timestamp={}", msg.getJMSMessageID, msg.getJMSTimestamp : JLong)
				debug1("ThingActReceiverBinary-JMSListener - received msg, dumping to see if 'wacky' headers show up:\n{}", msg)
				msg match {
					case objMsg: JMSObjMsg => {
						info1("Listener processing received objMsg with tstamp={}", objMsg.getJMSTimestamp: JLong)
						receiveJSerBinaryMsg(objMsg)
					}
					case other => {
						error2("Received unexpected (not JMS-ObjectMessage) message, class={}, dump=\n{}", other.getClass,  other)
					}
				}
			}
		}
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
	val topicName_forJSerBinTA = "thingActSpec_jserBin"
	val topicName_forTurtleTxtTA = "thingActSpec_turtleTxt"

	val allTopics = List(topicName_forJSerBinTA, topicName_forTurtleTxtTA)

}
abstract class FrienduActor() extends Actor with VarargsLogging
// Seems that using "with ActorLogging" here leads
class TATestDummyActor(argGoesHere : String) extends FrienduActor {
	getLogger.info("In dummy actor constructor, arg={}", argGoesHere)
	def receive = {
		case msg: AnyRef => {
			val msgDump = msg.toString()
			getLogger.info("TATDA received msg of clazz={} and dump-len={}", msg.getClass, msgDump.length)
			getLogger.debug("Received message dump:\n{}", msgDump)
		}
	}
}

class QPidTestEndpoint(myQPidConnMgr : QPidTopicConn_032) {
	lazy val myJmsSession = myQPidConnMgr.makeSession

	lazy val destForTATxtMSg : JMSDestination = myQPidConnMgr.myDestsByNameTail.get(TestAppNames.topicName_forTurtleTxtTA).get
	lazy val destForTABinMSg : JMSDestination = myQPidConnMgr.myDestsByNameTail.get(TestAppNames.topicName_forJSerBinTA).get
}

class TestTAQpidServer(myParentARF : ActorRefFactory, qpidConnMgr : QPidTopicConn_032)
			extends QPidTestEndpoint(qpidConnMgr) {

	lazy val myConsumer_forTurtleTxt : JMSMsgConsumer = myJmsSession.createConsumer(destForTATxtMSg)
	lazy val myConsumer_forJSerBin : JMSMsgConsumer = myJmsSession.createConsumer(destForTABinMSg)

	val rcvActor = makeTestDummyActor(myParentARF, "dummy-goody-rq-rcvr")
	val rcvWeakTeller = new ActorRefCPMsgTeller(rcvActor)

//	val rcvrFactory = new RecvrFactory
//	val allPurposeListener = rcvrFactory.makeItWeakerButEasier(rcvWeakTeller)

	val rcvrTxt : ThingActReceiverTxt = new ThingActReceiverTxt(rcvWeakTeller.asInstanceOf[CPStrongTeller[VWGoodyRqRdf]])
	val rcvrBin : ThingActReceiverBinary = new ThingActReceiverBinary(rcvWeakTeller.asInstanceOf[CPStrongTeller[VWGoodyRqActionSpec]])

	myConsumer_forTurtleTxt.setMessageListener(rcvrTxt.makeListener)
	myConsumer_forJSerBin.setMessageListener(rcvrBin.makeListener)

	def makeTestDummyActor(parentARF : ActorRefFactory, dummyActorName : String) : ActorRef = {
		val argInstruct = """This constructor arg could be any java object,
				but should be java-serializable if used in dynamic network context (see akka docs)."""
		val dummyActorProps = Props(classOf[TATestDummyActor], argInstruct)
		val dummyActorRef : ActorRef = parentARF.actorOf(dummyActorProps, dummyActorName)
		dummyActorRef
	}

}
import scala.collection.JavaConverters._
class TestTAQPidClient(qpidConnMgr : QPidTopicConn_032) extends QPidTestEndpoint(qpidConnMgr) {

	val myProdForTurtle = myJmsSession.createProducer(destForTATxtMSg)
	val myProdForJSer = myJmsSession.createProducer(destForTABinMSg)

	val mySender = new ThingActSenderQPid(myJmsSession, myProdForJSer, myProdForTurtle)

	def sendThingAct(taSpec : ThingActionSpec, encodePref : Integer): Unit = mySender.postThingAct(taSpec, encodePref)

	def sendSomeMsgs : Unit = {
		val gtmm: GoodyTestMsgMaker = new GoodyTestMsgMaker
		val msgsJList = gtmm.makeGoodyCreationMsgs
		var msgCount = 0
		val msgSList : List[ThingActionSpec] = msgsJList.asScala.toList
		for (msg <- msgSList) {
			sendThingAct(msg, msgCount % 3)
			msgCount += 1
			Thread.sleep(1200)
		}

	}

}
object ThingActMoverQPid_UnitTest extends VarargsLogging {
	val akkaSysName = "unit-test-ta-qpid-mover"
	lazy private val myServerAkkaSys = ActorSystem(akkaSysName)

	def main(args: Array[String]) : Unit = {
		info1("TAMover test started with cmd-line-args array={}", args)

		val qpidConnMgr = new QPidTopicConn_032(TestAppNames.allTopics)

		info1("QPidConnMgr.DestMap={}", qpidConnMgr.myDestsByNameTail)

		// server and client are in same Java process, same qpid-conn, but separate JMSSessions.
		val server = new TestTAQpidServer(myServerAkkaSys, qpidConnMgr)

		val client = new TestTAQPidClient(qpidConnMgr)

		client.sendSomeMsgs
	}

}

