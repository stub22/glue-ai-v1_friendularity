package org.friendularity.qpc

import javax.jms.{Destination => JMSDestination, Message => JMSMsg, MessageConsumer => JMSMsgConsumer,
MessageListener => JMSMsgListener, MessageProducer => JMSMsgProducer, ObjectMessage => JMSObjMsg,
Session => JMSSession, TextMessage => JMSTextMsg}

import org.friendularity.vw.msg.cor.VWorldNotice

/**
  * Created by Stub22 on 8/8/2016.
  */

case class FunVWNoticeImpl(funTxt : String) extends VWorldNotice

trait VWNoticeSender {
	def sendVWNotice(vwNotice : VWorldNotice)
	def sendPingNotice(txt : String): Unit = {
		val pingNotice = new FunVWNoticeImpl(getClass.getName + " sends Ping Notice: " + txt)
		sendVWNotice(pingNotice)
	}
}
class VWNoticeSenderJmsImpl(myJmsSess : JMSSession, myJmsProd_JSer : JMSMsgProducer) extends VWNoticeSender with KnowsJmsSession {
	override def getJmsSession : JMSSession = myJmsSess
	val myHdrWriter : Option[WritesJmsHeaders] = None
	lazy val myNoticeSendChan : DefinedJmsSenderChan = new JmsSenderChanImpl(myJmsSess, myJmsProd_JSer, myHdrWriter)
	override def sendVWNotice(vwNotice : VWorldNotice) : Unit = {
		myNoticeSendChan.sendJmsObjMsg(vwNotice)
	}
}
