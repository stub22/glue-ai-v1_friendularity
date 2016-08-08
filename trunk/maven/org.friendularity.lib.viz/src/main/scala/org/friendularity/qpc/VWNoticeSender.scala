package org.friendularity.qpc

import org.friendularity.vwmsg.VWorldNotice

import javax.jms.{Destination => JMSDestination, Message => JMSMsg, MessageConsumer => JMSMsgConsumer,
MessageListener => JMSMsgListener, MessageProducer => JMSMsgProducer, ObjectMessage => JMSObjMsg,
Session => JMSSession, TextMessage => JMSTextMsg}

/**
  * Created by Stub22 on 8/8/2016.
  */

case class FunVWNoticeImpl(funTxt : String) extends VWorldNotice

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
