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
import java.lang.{Long => JLong, Integer => JInt}
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

class DummyTestHeaderWriter() extends WritesJmsHeaders {
	override def putHeadersOnMsg(msg : JMSMsg, dataInside : Any): Unit = {
		msg.setIntProperty("wackyInt", 987654321);
		msg.setStringProperty("wackyName", "Widget");
		msg.setDoubleProperty("wackyPrice", 0.99);
	}
}


trait ClientStatListenerCouldUseAkkaButThatWouldBeWeird extends  DummyActorMaker {
	val myParentARF : ActorRefFactory = ???
	val wwStatRcvActor = makeTestDummyActor(myParentARF, "vwStatRcvr")
	val vwStatWeakTeller = new ActorRefCPMsgTeller(wwStatRcvActor)
}

// Test far-outer client ability to receive VWorld status and dump the message contents.
// A real external client can do something more fun or useful with this data.
trait ExoPubStatDumpingListenerMaker extends VarargsLogging {
	// statDumpPeriod <= 0   means no stat dumps
	def makePubStatDumpingListener(statDumpPeriod : Int) : JMSMsgListener = {
		new JMSMsgListener() {
			var myStatusRcvdCnt = 0
			override def onMessage(msg: JMSMsg): Unit = {
				debug2("VWPubStatListener-JMSListener msgID={} timestamp={}", msg.getJMSMessageID, msg.getJMSTimestamp : JLong)
				debug1("VWPubStatListener-JMSListener - dumping rcvd msg, to see if 'wacky' headers show up:\n{}", msg)
				msg match {
					case objMsg: JMSObjMsg => {
						myStatusRcvdCnt += 1
						if ((statDumpPeriod > 0) && ((myStatusRcvdCnt % statDumpPeriod) == 0)) {
							val objCont: JSerializable = objMsg.getObject
							info4("VWPubStatListener received {}th update, msgID={}, tstamp={}, notice={}", myStatusRcvdCnt : JInt,
								objMsg.getJMSMessageID, objMsg.getJMSTimestamp: JLong, objCont.asInstanceOf[AnyRef])
						}
					}
					case otherMsg => {
						error2("Received unexpected (not JMS-ObjectMessage) message, class={}, dump=\n{}", otherMsg.getClass,  otherMsg)
					}
				}
			}
		}
	}
}

// Runs both a "server" and a "client" in one process, to verify that basic duplex message flow works.
// This would get deserialization errors in both directions, if run under OSGi.
object ThingActMoverQPid_UnitTest extends VarargsLogging {
	val srvrAkkaSysName = "unit-test-ta-qpid-vwSrvr"
	lazy private val myServerAkkaSys = ActorSystem(srvrAkkaSysName)
	// To have any more akka-sys instances, we would need separate akka-remote port numbers.
	// Our client test does *not* use akka, it is a simple JVM client listening to JMS

	def main(args: Array[String]) : Unit = {
		info1("TAMover test started with cmd-line-args array={}", args)

		//  JNDI works in standalone unit tests, but not so well under OSGi
		// val qpidTopicMgr : QpidTopicConn = new QPidTopicConn_JNDI_032(TestAppNames.allTopics)

		val qpidConnMgr : QpidConnMgr = new QpidConnMgrJFlux

		// info1("QPidConnMgr.DestMap={}", qpidConnMgr.getDestsByNameTail)

		// server and client are in same Java process, same qpid-conn, but separate JMSSessions.
		val server = new TestTAQpidServer(myServerAkkaSys, qpidConnMgr)

		val clientDestMgr : QpidDestMgr = new QPidDestMgrJFlux(qpidConnMgr)

		val client = new TestTAQPidClient(clientDestMgr)

		qpidConnMgr.startConn

		client.sendSomeVWRqs(500) // sends a mixture of bin-serial and turtle-txt TAs, which we see receieved in server

		val vwNoticeSender = server.getServerPublishFeature.getVWPubNoticeSender
		sendSomeVWNotices_Blocking(vwNoticeSender, 25, 450) // Sends bin-serial notices out, which we see received in client

		server.installDumpingListeners()

	}
	def sendSomeVWNotices_Blocking(sender : VWNoticeSender, numNotices : Int, sleepIntervMsec : Int) : Unit = {
		var msgCount = 0
		while (msgCount < numNotices) {
			msgCount += 1
			sender.sendPingNotice("number = " + msgCount)
			Thread.sleep(sleepIntervMsec)
		}
	}

}

