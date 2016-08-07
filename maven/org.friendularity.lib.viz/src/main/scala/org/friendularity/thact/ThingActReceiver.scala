package org.friendularity.thact

import java.lang.{Long => JLong}
import javax.jms.{Destination => JMSDestination, Message => JMSMsg, MessageConsumer => JMSMsgConsumer, MessageListener => JMSMsgListener, MessageProducer => JMSMsgProducer, ObjectMessage => JMSObjMsg, Session => JMSSession, TextMessage => JMSTextMsg}

import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.api.thing.ThingActionSpec
import org.friendularity.cpmsg.CPStrongTeller
import org.friendularity.vwmsg.{VWorldNotice, VWGoodyRqTAS, VWGoodyRqActionSpec, VWGoodyRqTurtle, VWGoodyRqRdf}

/**
  * Created by Stub22 on 8/6/2016.
  */
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
class VWStatReceiverBinary(statusTeller : CPStrongTeller[VWorldNotice]) extends VarargsLogging  {
	def receiveJSerBinaryMsg(objMsg : JMSObjMsg) : Unit = {
		val objCont = objMsg.getObject
		val statNotice : VWorldNotice = objCont.asInstanceOf[VWorldNotice]
		statusTeller.tellStrongCPMsg(statNotice)
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
