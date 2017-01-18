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


package org.friendularity.thact

import java.lang.{Long => JLong, Integer => JInt}
import javax.jms.{Destination => JMSDestination, Message => JMSMsg, MessageConsumer => JMSMsgConsumer, MessageListener => JMSMsgListener, MessageProducer => JMSMsgProducer, ObjectMessage => JMSObjMsg, Session => JMSSession, TextMessage => JMSTextMsg}

import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.api.thing.ThingActionSpec
import org.friendularity.cpmsg.CPStrongTeller
import org.friendularity.vw.msg.cor.VWorldNotice
import org.friendularity.vwmsg.{VWRqTAWrapper, VWRqTAWrapImpl,  VWTARqTurtle, VWTARqRdf}

/**
  * Created by Stub22 on 8/6/2016.
  */
// JMS spec requires Listener's onMessage method to only be called serially [...although if we attach same listener to
// multiple consumers, probably can be called concurrently, eh?].   We don't really need that invariant
// here, cince we just statelessly forward the message to an actor destination.
// Impl Notes:
// This listener sniffs at the inbound JMS message to find type, and dispatches to Receiver code.
// Receiver code wraps and sends an appropriate actor msg to enqueue further work on the message.

trait JmsListenerMaker {
	def makeListener : JMSMsgListener
}
trait TurtleTAMsgDecoder extends JenaModelReader with VarargsLogging {
	def decodeTAJmsTurtleMsg(jmsTxtMsg : JMSTextMsg) : Traversable[ThingActionSpec] = {
		val txtCont: String = jmsTxtMsg.getText
		decodeTATurtleTxtModel(txtCont)
	}
	// Currently we log a warning if TA count != 1, then process all TAs.   Can refine that behavior as needed.
	def decodeTATurtleTxtModel(modelTxt : String) : Traversable[ThingActionSpec] = {

		val decodeFlags_opt = None
		val jenaModel = readModelFromTurtleTxt(modelTxt, decodeFlags_opt)
		val exposer = new ThingActExposer {}
		val thingActs: List[ThingActionSpec] = exposer.extractThingActsFromModel(jenaModel)

		if (thingActs.isEmpty) {
			warn1("Found 0 ThingActs in inbound jmsTxt-message, dumping model:\n {}", jenaModel)
		}
		if (thingActs.length > 1) {
			warn1("Found {} ThingActs in inbound jmsTxt-message, processing in arbitrary order (TODO: sort by timestamp)", thingActs.length: Integer)
		}
		thingActs
	}

}
class ThingActReceiverTxt(taTurtleTeller : CPStrongTeller[VWTARqRdf]) extends
			JmsListenerMaker with TurtleTAMsgDecoder {

	private def forwardUndecodedGoodyTxtMsg(txtMsg : JMSTextMsg) : Unit = {
		val txtCont = txtMsg.getText
		// We assume it is a turtle encoding of ThingActSpec(s), targeting VW-Goodies.
		// But what if it is a body-manip or cam-manip request?
		val goodyTATurtleRq = new VWTARqTurtle(txtCont)
		taTurtleTeller.tellStrongCPMsg(goodyTATurtleRq)
	}
	override def makeListener : JMSMsgListener = {
		new JMSMsgListener() {
			override def onMessage(msg: JMSMsg): Unit = {
				debug2("ThingActReceiverTxt-JMSListener msgID={} timestamp={}", msg.getJMSMessageID, msg.getJMSTimestamp : JLong)
				debug1("ThingActReceiverTxt-JMSListener - received msg, dumping to see if 'wacky' headers show up:\n{}", msg)
				msg match {
					case txtMsg: JMSTextMsg => {
						info2("JMSListener processing received txtMsg with length={} and tstamp={}", txtMsg.getText.length : JInt,  txtMsg.getJMSTimestamp: JLong)
						forwardUndecodedGoodyTxtMsg(txtMsg)
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
class ThingActReceiverBinary(taDirectTeller : CPStrongTeller[VWRqTAWrapper])
			extends JmsListenerMaker with VarargsLogging {

	def receiveJSerBinaryMsg(objMsg : JMSObjMsg) : Unit = {
		val objCont = objMsg.getObject // This call does the deserial, will fail if classpath not correct.
		val taSpec: ThingActionSpec = objCont.asInstanceOf[ThingActionSpec] // will fail if class not as expected
		receiveThingAction(taSpec)
	}
	def receiveThingAction(taSpec: ThingActionSpec) {
		val verbID = taSpec.getVerbID
		val targetID = taSpec.getTargetThingID
		val targetTypeID = taSpec.getTargetThingTypeID
		info3("forwarding TA to taTeller for target={}, targetType={}, verb={}", targetID, targetTypeID, verbID)
		forwardMsgToThingActionActor(taSpec)
	}
	def forwardMsgToThingActionActor(taSpec: ThingActionSpec) : Unit = {
		val taDirectRq = new VWRqTAWrapImpl(taSpec) // make a new wrapper message to pass along
		taDirectTeller.tellStrongCPMsg(taDirectRq)
	}
	override def makeListener : JMSMsgListener = {
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

class ThingActReceiverDual(myTARcvBin : ThingActReceiverBinary) extends JmsListenerMaker with TurtleTAMsgDecoder {

	private def receiveTAJmsTurtleMsg(jmsTxtMsg : JMSTextMsg) : Int = {
		val thingActs = decodeTAJmsTurtleMsg(jmsTxtMsg)
		for (thAct <- thingActs) {
			myTARcvBin.receiveThingAction(thAct)
		}
		thingActs.size
	}

	override def makeListener: JMSMsgListener = {
		new JMSMsgListener() {
			override def onMessage(msg: JMSMsg): Unit = {
				info2("ThingActReceiverDual-JMSListener rcvd msgID={} timestamp={}", msg.getJMSMessageID, msg.getJMSTimestamp: JLong)
				msg match {
					case txtMsg: JMSTextMsg => {
						val taCount = receiveTAJmsTurtleMsg(txtMsg)
						info2("Processed {} taMsgs from turtle blob of length={}, onMessage is now finished!",
									taCount : JInt, txtMsg.getText.length : JInt)
					}
					case objMsg: JMSObjMsg => {
						info1("Dual-Listener processing received objMsg with tstamp={}", objMsg.getJMSTimestamp: JLong)
						myTARcvBin.receiveJSerBinaryMsg(objMsg)
					}
					case other => {
						error2("Received unexpected (not JMS-ObjectMsg nor JMS-TextMsg) message, class={}, dump=\n{}", other.getClass,  other)
					}
				}


			}
		}
	}
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
