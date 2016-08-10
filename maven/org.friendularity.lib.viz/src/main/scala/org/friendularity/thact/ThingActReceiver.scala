package org.friendularity.thact

import java.lang.{Long => JLong, Integer => JInt}
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
		// But what if it is a body-manip or cam-manip request?
		val goodyTATurtleRq = new VWGoodyRqTurtle(txtCont)
		goodyTATurtleTeller.tellStrongCPMsg(goodyTATurtleRq)
	}
	def makeListener : JMSMsgListener = {
		new JMSMsgListener() {
			override def onMessage(msg: JMSMsg): Unit = {
				debug2("ThingActReceiverTxt-JMSListener msgID={} timestamp={}", msg.getJMSMessageID, msg.getJMSTimestamp : JLong)
				debug1("ThingActReceiverTxt-JMSListener - received msg, dumping to see if 'wacky' headers show up:\n{}", msg)
				msg match {
					case txtMsg: JMSTextMsg => {
						info2("JMSListener processing received txtMsg with length={} and tstamp={}", txtMsg.getText.length : JInt,  txtMsg.getJMSTimestamp: JLong)
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
// Note that if this receiver is running under OSGi, it must have correct classpath in scope (thrdCtx?) during deserial.
class ThingActReceiverBinary(goodyTADirectTeller : CPStrongTeller[VWGoodyRqActionSpec]) extends VarargsLogging  {
	def receiveJSerBinaryMsg(objMsg : JMSObjMsg) : Unit = {
		val objCont = objMsg.getObject // This call does the deserial, will fail if classpath not correct.
		val taSpec : ThingActionSpec = objCont.asInstanceOf[ThingActionSpec] // will fail if class not as expected
		val goodyTADirectRq = new VWGoodyRqTAS(taSpec) // make a new wrapper message to pass along
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
	// TODO:    makeConversionListenTeller
}

// Note that if this receiver is running under OSGi, it must have correct classpath in scope (thrdCtx?) during deserial.
class VWStatReceiverBinary(statusTeller : CPStrongTeller[VWorldNotice]) extends VarargsLogging  {
	def receiveJSerBinaryMsg(objMsg : JMSObjMsg) : Unit = {
		val objCont = objMsg.getObject // does deserial, requiring correct classpath
		val statNotice : VWorldNotice = objCont.asInstanceOf[VWorldNotice] // strong type assumption applied, could fail
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
