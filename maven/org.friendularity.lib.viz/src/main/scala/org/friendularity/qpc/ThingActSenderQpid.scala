package org.friendularity.qpc

import java.io.{Serializable => JSerializable}
import java.lang.{Long => JLong}
import javax.jms.{Destination => JMSDestination, Message => JMSMsg, MessageConsumer => JMSMsgConsumer,
		MessageListener => JMSMsgListener, MessageProducer => JMSMsgProducer, ObjectMessage => JMSObjMsg,
		Session => JMSSession, TextMessage => JMSTextMsg}

import org.cogchar.api.thing.ThingActionSpec
import org.friendularity.thact.{ThingActTurtleEncoder, ThingActSender}

/**
  * Created by Stub22 on 8/8/2016.
  * Fetched through MakesVWTARqProducers
  * When "unified", then both JMS-producers are the same, but header writers can still be different.
  */
class ThingActSenderQPid(myJmsSess : JMSSession, myJmsProd_JSer : JMSMsgProducer, serHdrWrtr : Option[WritesJmsHeaders],
						 myJmsProd_Turtle : JMSMsgProducer, trtHdrWrtr : Option[WritesJmsHeaders])
			extends ThingActSender  with KnowsJmsSession {

	override def getJmsSession : JMSSession = myJmsSess

	lazy val mySendChan_TAJSer : DefinedJmsSenderChan = new JmsSenderChanImpl(myJmsSess, myJmsProd_JSer, serHdrWrtr)
	lazy val mySendChan_TATurtle : DefinedJmsSenderChan = new JmsSenderChanImpl(myJmsSess, myJmsProd_Turtle, trtHdrWrtr)

	lazy val myTurtleEncoder = new ThingActTurtleEncoder{}

	override def supportsJavaSer : Boolean = true
	override def supportsTurtleSer : Boolean = true

	override def postThingActViaJavaSer(srlzblTASpec : ThingActionSpec): Unit = {
		info1("Sending java-ser TA msg for action={}", srlzblTASpec.getActionSpecID)
		mySendChan_TAJSer.sendJmsObjMsg(srlzblTASpec.asInstanceOf[JSerializable])

	}
	override def postThingActViaTurtleSer(taSpec : ThingActionSpec): Unit = {
		info1("Sending turtle-txt TA msg for action={}", taSpec.getActionSpecID)
		val taAsTurtleTxt : String  = myTurtleEncoder.encodeAsTurtleMsg(taSpec)
		mySendChan_TATurtle.sendJmsTxtMsg(taAsTurtleTxt)

	}
}
// Java-friendly class, offering turtle-encoding only, for use in student environments.
class TASenderQPidTurtleTxt(jmsSess : JMSSession, jmsProd_Turtle : JMSMsgProducer,
							jmsHdrWrtr_orNull : WritesJmsHeaders)
		extends ThingActSenderQPid(jmsSess, null, None, jmsProd_Turtle, Option(jmsHdrWrtr_orNull)) {

	override def supportsJavaSer: Boolean = false

	override def postThingActViaJavaSer(srlzblTASpec: ThingActionSpec): Unit = ???

}
