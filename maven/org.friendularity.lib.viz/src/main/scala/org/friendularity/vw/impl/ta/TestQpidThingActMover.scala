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

package org.friendularity.vw.impl.ta

import java.io.{Serializable => JSerializable}
import java.lang.{Integer => JInt, Long => JLong}
import javax.jms.{Destination => JMSDestination, Message => JMSMsg, MessageConsumer => JMSMsgConsumer, MessageListener => JMSMsgListener, MessageProducer => JMSMsgProducer, ObjectMessage => JMSObjMsg, Session => JMSSession, TextMessage => JMSTextMsg}

import akka.actor.ActorSystem
import org.appdapter.fancy.log.VarargsLogging
import org.friendularity.netcli.vwta.{DummyGoodySender, QPidTATestClient}
import org.friendularity.infra.qpc.{JmsDestMgr, QPidDestMgrJFlux, QpidConnMgr, QpidConnMgrJFlux}
import org.friendularity.qth.VWNoticeSender

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


// Runs both a "server" and a "client" in one process, to verify that basic duplex message flow works.
// This would get deserialization errors in both directions, if run under OSGi.
object TestQpidThingActMover extends VarargsLogging {
	val srvrAkkaSysName = "unit-test-ta-qpid-vwSrvr"
	lazy private val myServerAkkaSys = ActorSystem(srvrAkkaSysName)
	// To have any more akka-sys instances, we would need separate akka-remote port numbers.
	// Our client test does *not* use akka, it is a simple JVM client listening to JMS

	def main(args: Array[String]) : Unit = {
		info1("TAMover test started with cmd-line-args array={}", args)

		//  JNDI works in standalone unit tests, but not so well under OSGi
		// val qpidTopicMgr : QpidTopicConn = new QPidTopicConn_JNDI_032(TestAppNames.allTopics)

		val srvrConnMgr : QpidConnMgr = new QpidConnMgrJFlux
		val clientConnMgr : QpidConnMgr = new QpidConnMgrJFlux

		// info1("QPidConnMgr.DestMap={}", qpidConnMgr.getDestsByNameTail)

		// server and client are in same Java process, same qpid-conn, but separate JMSSessions.
		val server = new TestTAQpidServer(myServerAkkaSys, srvrConnMgr)

		val clientDestMgr : JmsDestMgr = new QPidDestMgrJFlux(clientConnMgr)

		val client = new QPidTATestClient(clientDestMgr)

		info0("\nStarting SERVER Conn (listens for TARqs, publishes stat-notices)")
		srvrConnMgr.startConn

		info0("\nStarting CLIENT Conn (sends TA-Rqs  listens for stat-notices")
		clientConnMgr.startConn

		val dummyGoodySender = new DummyGoodySender {}
		// We send some messages before the server is listening, to see if it picks them up after listening starts.
		dummyGoodySender.slowlySendSomeVWRqs(client, 500) // sends a mixture of bin-serial and turtle-txt TAs, which we see receieved in server

		val vwNoticeSender = server.getServerPublishFeature.getVWPubNoticeSender
		sendSomeVWNotices_Blocking(vwNoticeSender, 25, 450) // Sends bin-serial notices out, which we see received in client

		info0("\nWill install server listeners, then sleep 2 sec.")
		server.installDumpingListeners()
		Thread.sleep(2000)
		info0("\nWoke up and will send some more client msgs.")
		dummyGoodySender.slowlySendSomeVWRqs(client, 300)
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

