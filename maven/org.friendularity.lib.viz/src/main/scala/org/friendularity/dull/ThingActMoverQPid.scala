package org.friendularity.dull

import javax.jms.{Session => JMSSession, Message => JMSMsg, ObjectMessage => JMSObjMsg, TextMessage => JMSTextMsg,
		MessageProducer => JMSMsgProducer, Destination => JMSDestination, MessageConsumer => JMSMsgConsumer}

import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.api.thing.ThingActionSpec
import java.io.{Serializable => JSerializable}

import org.friendularity.ignore.nexjen.{QPidConnector, QPid_032_NameManager}

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
class QPidTopicConn_032(myTopicExchangeNameList : List[String]) {
	lazy val myNameMgr = new QPid_032_NameManager()

	lazy val myJndiProps = myNameMgr.makeJndiPropsForTopicSetup(myTopicExchangeNameList)

	lazy val myQPidConn : QPidConnector = {
		val qc = new QPidConnector(myJndiProps)
		qc.startConn()
		qc
	}

	lazy val myJmsSession = myQPidConn.makeSessionAutoAck()

	lazy val myDestMap : Map[String, JMSDestination] = myTopicExchangeNameList.map(n => n -> myQPidConn.makeDestination(n)).toMap

	/*
	info0("================= Creating Producer")
	val jmsProducer_001 = jmsSession.createProducer(jmsDest_001);
	info0("================= Creating Consumer")
	val jmsConsumer_001 = jmsSession.createConsumer(jmsDest_001);
	*/

}
class ThingActMoverQPid {

}
