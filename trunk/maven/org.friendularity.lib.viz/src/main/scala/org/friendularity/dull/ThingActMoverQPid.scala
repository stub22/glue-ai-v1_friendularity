package org.friendularity.dull

import javax.jms.{Session => JMSSession, Message => JMSMsg, ObjectMessage => JMSObjMsg, TextMessage => JMSTextMsg,
		MessageProducer => JMSMsgProducer, Destination => JMSDestination, MessageConsumer => JMSMsgConsumer,
		MessageListener => JMSMsgListener}

import akka.actor.{ActorSystem, Actor, ActorLogging, Props, ActorRef, ActorRefFactory}
import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.api.thing.ThingActionSpec
import java.io.{Serializable => JSerializable}

import org.cogchar.render.goody.basic.BasicGoodyCtx
import org.friendularity.cpump.{ActorRefCPMsgTeller, CPMsgTeller, CPStrongTeller}
import org.friendularity.ignore.nexjen.{QPidConnector, QPid_032_NameManager}
import org.friendularity.respire.{VWorldRequest, VWGoodyActor, VWGoodyRqTAS, VWGoodyRqActionSpec, VWGoodyRqRdf, VWGoodyRqTurtle}

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

trait SenderQPid {
	protected def getJmsSession : JMSSession
	protected def getJmsProducer : JMSMsgProducer
	protected def putHeadersOnMsg(msg : JMSMsg): Unit

	def sendJavaSer(objToSend : JSerializable): Unit = {
		val jmsSession = getJmsSession
		val objMsg_toSend : JMSObjMsg = jmsSession.createObjectMessage
		objMsg_toSend.setObject(objToSend)
		putHeadersOnMsg(objMsg_toSend)
		val jmsProd = getJmsProducer
		jmsProd.send(objMsg_toSend)
	}

	def sendString(strToSend : String) : Unit = {
		val jmsSession = getJmsSession
		val txtMsg_toSend : JMSTextMsg = jmsSession.createTextMessage(strToSend)
		putHeadersOnMsg(txtMsg_toSend)
		val jmsProd = getJmsProducer
		jmsProd.send(txtMsg_toSend)
	}

}

class ThingActSenderQPid(myJmsSess : JMSSession, myJmsProd : JMSMsgProducer)
					extends ThingActSender with SenderQPid {

	override protected def getJmsSession : JMSSession = myJmsSess
	override protected def getJmsProducer : JMSMsgProducer = myJmsProd


	override def postThingActViaJavaSer(taSpec : ThingActionSpec): Unit = {
		sendJavaSer(taSpec.asInstanceOf[JSerializable])
	}
	override protected def putHeadersOnMsg(msg : JMSMsg): Unit = {
		msg.setIntProperty("wackyInt", 987654321);
		msg.setStringProperty("wackyName", "Widget");
		msg.setDoubleProperty("wackyPrice", 0.99);
	}


}
class ThingActReceiverTxt(goodyTATurtleTeller : CPStrongTeller[VWGoodyRqRdf]) {
	def receiveTextMsg(txtMsg : JMSTextMsg) : Unit = {
		val txtCont = txtMsg.getText
		// We assume it is a turtle encoding of ThingActSpec(s), targeting VW-Goodies.
		val goodyTATurtleRq = new VWGoodyRqTurtle(txtCont)
		goodyTATurtleTeller.tellStrongCPMsg(goodyTATurtleRq)
	}
}
class ThingActReceiverBinary(goodyTADirectTeller : CPStrongTeller[VWGoodyRqActionSpec]) {
	def receiveJSerBinaryMsg(objMsg : JMSObjMsg) : Unit = {
		val objCont = objMsg.getObject
		val taSpec : ThingActionSpec = objCont.asInstanceOf[ThingActionSpec]
		val goodyTADirectRq = new VWGoodyRqTAS(taSpec)
		goodyTADirectTeller.tellStrongCPMsg(goodyTADirectRq)
	}
}
class ThingActJmsListener(rcvrTxt : ThingActReceiverTxt, rcvrBin : ThingActReceiverBinary)
			extends JMSMsgListener with VarargsLogging {
	// JMS spec requires this method to only be called serially [...although if we attach same listener to
	// multiple consumers, probably can be called concurrently, eh?].   We don't really need that invariant
	// here, cince we just statelessly forward the message to an actor destination.
	// Impl Notes:
	// This listener sniffs at the inbound JMS message to find type, and dispatches to Receiver code.
	// Receiver code wraps and sends an appropriate actor msg to enqueue further work on the message.
	override def onMessage(msg: JMSMsg): Unit = {
		info1("Received msg, dumping to see if 'wacky' headers show up:\n{}", msg)
		msg match {
			case txtMsg : JMSTextMsg => {
				rcvrTxt.receiveTextMsg(txtMsg)
			}
			case objMsg : JMSObjMsg => {
				rcvrBin.receiveJSerBinaryMsg(objMsg)
			}
			case other => {
				error1("Received unexpected message: {}", other)
			}

		}
	}
}
class RecvrFactory {
	def makeThingActJMSListener(turtleTeller : CPStrongTeller[VWGoodyRqRdf],
						   	jserDirectTeller : CPStrongTeller[VWGoodyRqActionSpec]) : JMSMsgListener = {
		val rcvrTxt : ThingActReceiverTxt = new ThingActReceiverTxt(turtleTeller)
		val rcvrBin : ThingActReceiverBinary = new ThingActReceiverBinary(jserDirectTeller)
		new ThingActJmsListener(rcvrTxt, rcvrBin)
	}

	def makeItWeakerButEasier(oneWeakTeller : CPMsgTeller) : JMSMsgListener = {
		makeThingActJMSListener(oneWeakTeller.asInstanceOf[CPStrongTeller[VWGoodyRqRdf]],
			oneWeakTeller.asInstanceOf[CPStrongTeller[VWGoodyRqActionSpec]])
	}
}
class QPidTopicConn_032(myTopicExchangeNameList : List[String]) {
	lazy val myNameMgr = new QPid_032_NameManager()

	lazy val myJndiProps = myNameMgr.makeJndiPropsForTopicSetup(myTopicExchangeNameList)

	lazy val myQPidConn : QPidConnector = {
		val qc = new QPidConnector(myJndiProps)
		qc.startConn()
		qc
	}

	def makeSession : JMSSession = myQPidConn.makeSessionAutoAck()

	lazy val myDestMap : Map[String, JMSDestination] = myTopicExchangeNameList.map(n => n -> myQPidConn.makeDestination(n)).toMap

	/*
	info0("================= Creating Producer")
	val jmsProducer_001 = jmsSession.createProducer(jmsDest_001);
	info0("================= Creating Consumer")
	val jmsConsumer_001 = jmsSession.createConsumer(jmsDest_001);
	*/

}
object TestAppNames {
	// Nothing stops us from making these names the same, combining destinations, etc, if that's what we wanted.
	// But separate queues for separate formats is a decent approach.
	val topicName_forJSerBinTA = "thingActSpec_jserBin"
	val topicName_forTurtleTxtTA = "thingActSpec_turtleTxt"

	val allTopics = List(topicName_forJSerBinTA, topicName_forTurtleTxtTA)

}
class TATestDummyActor(argGoesHere : String) extends Actor with ActorLogging {
	log.info("In dummy actor constructor, arg={}", argGoesHere)
	def receive = {
		case msg: AnyRef => {
			log.info("TATDA received msg:\n{}", msg)
		}
	}
}

class QPidTestEndpoint(myQPidConnMgr : QPidTopicConn_032) {
	lazy val myJmsSession = myQPidConnMgr.makeSession

	lazy val destForTATxtMSg : JMSDestination = myQPidConnMgr.myDestMap.get(TestAppNames.topicName_forTurtleTxtTA).get
	lazy val destForTABinMSg : JMSDestination = myQPidConnMgr.myDestMap.get(TestAppNames.topicName_forJSerBinTA).get
}

class TestTAQpidServer(myParentARF : ActorRefFactory, qpidConnMgr : QPidTopicConn_032)
			extends QPidTestEndpoint(qpidConnMgr) {

	lazy val myConsumer_forTurtleTxt : JMSMsgConsumer = myJmsSession.createConsumer(destForTATxtMSg)
	lazy val myConsumer_forJSerBin : JMSMsgConsumer = myJmsSession.createConsumer(destForTABinMSg)

	val rcvActor = makeTestDummyActor(myParentARF, "dummy-goody-rq-rcvr")
	val rcvWeakTeller = new ActorRefCPMsgTeller(rcvActor)

	val rcvrFactory = new RecvrFactory
	val allPurposeListener = rcvrFactory.makeItWeakerButEasier(rcvWeakTeller)

	myConsumer_forTurtleTxt.setMessageListener(allPurposeListener)
	myConsumer_forJSerBin.setMessageListener(allPurposeListener)

	def makeTestDummyActor(parentARF : ActorRefFactory, dummyActorName : String) : ActorRef = {
		val argInstruct = """This constructor arg could be any java object,
				but should be java-serializable if used in dynamic network context (see akka docs)."""
		val dummyActorProps = Props(classOf[TATestDummyActor], argInstruct)
		val dummyActorRef : ActorRef = parentARF.actorOf(dummyActorProps, dummyActorName)
		dummyActorRef
	}

}
class TestClient(myQPidConnMgr : QPidTopicConn_032) {
	val myJmsSession = myQPidConnMgr.makeSession
}
object ThingActMoverQPid_UnitTest extends VarargsLogging {
	val akkaSysName = "unit-test-ta-qpid-mover"
	lazy private val myAkkaSys = ActorSystem(akkaSysName)

	def main(args: Array[String]) : Unit = {
		info1("TAMover test started with cmd-line-args array={}", args)

		val qpidConnMgr = new QPidTopicConn_032(TestAppNames.allTopics)

		val server = new TestTAQpidServer(myAkkaSys, qpidConnMgr)
	}


}

