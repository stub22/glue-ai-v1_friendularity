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
import java.lang.{Long => JLong}
import javax.jms.{Destination => JMSDestination, Message => JMSMsg, MessageConsumer => JMSMsgConsumer,
		MessageListener => JMSMsgListener, MessageProducer => JMSMsgProducer, ObjectMessage => JMSObjMsg,
		Session => JMSSession, TextMessage => JMSTextMsg}


import akka.actor.ActorRefFactory
import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.api.thing.ThingActionSpec
import org.cogchar.render.rendtest.GoodyTestMsgMaker
import org.friendularity.akact.{KnowsAkkaSys, DummyActorMaker}
import org.friendularity.cpmsg.{CPStrongTeller, ActorRefCPMsgTeller}
import org.friendularity.netcli.vwta.TestTAQPidClient
import org.friendularity.thact.{ThingActSender, ThingActReceiverBinary, ThingActReceiverTxt}
import org.friendularity.vwmsg.{VWRqTAWrapper, VWTARqRdf, VWorldNotice}

/**
  * Created by Stub22 on 8/8/2016.
  */

// KnowsJmsSession with
class QPidFeatureEndpoint(myJmsDestMgr : QpidDestMgr) extends  KnowsDestMgr {
	// lazy val myJmsSession = myJmsDestMgr.makeSession

	override def getDestMgr : QpidDestMgr = myJmsDestMgr
}


trait HasQpidConn extends VarargsLogging {
	private lazy val myQpidConnMgr : QpidConnMgr = new QpidConnMgrJFlux

	def getQpidConnMgr : QpidConnMgr = myQpidConnMgr

	var myConnStartedFlag : Boolean = false

	def startQpidConn : Unit = {
		if (!myConnStartedFlag) {
			myQpidConnMgr.startConn
			myConnStartedFlag = true
		} else {
			warn1("Qpid connection was already started for {}, ignoring extra request to do so", this)
		}
	}

}
trait OffersQpidSomething extends VarargsLogging {
	protected lazy val myQpidConnHaver = new HasQpidConn{}
	protected lazy val myQpidConnMgr = myQpidConnHaver.getQpidConnMgr
	def startQpidConn: Unit = {
		myQpidConnMgr.startConn
	}
}

trait OffersVWorldServer extends KnowsAkkaSys with OffersQpidSomething  {
	// lazy val qpidTopicMgr : QpidTopicConn = new QPidTopicConn_JNDI_032(TestAppNames.allTopics)

	// Under OSGi the server and client cannot easily deserialize messages with JMSMessage.getObject
	// (Needs classpath wiring help - would it help to set context-cl during onMessage handler?).
	// However they can *make* serialized messages OK, and send them out for consumption by
	// simpler Java clients1.  Inside OSGi we can also read turtle-text messages, avro messages,
	// JMSText message, JMSMap messages, etc.
/*
     [java] 30399   INFO [Dispatcher-1-Conn-1] org.friendularity.qpc.TestTAQPidClient (HasLogger.scala:31) info2 - VWPubStatListener-JMSListener msgID=ID:866e1fa5-e6c2-33f4-9770-4f3e8bffecfd timestamp=1470709600728
     [java] 30400  ERROR [Dispatcher-1-Conn-1] org.apache.qpid.client.BasicMessageConsumer (BasicMessageConsumer.java:795) notifyMessage - reNotification : Caught exception (dump follows) - ignoring...
     [java] javax.jms.MessageFormatException: Unable to deserialize message
     [java] 	at org.apache.qpid.client.message.JMSObjectMessage.getObject(JMSObjectMessage.java:160)
     [java] 	at org.friendularity.qpc.VWPubStatListenerMaker$$anon$1.onMessage(ThingActMoverQPid.scala:86)
     [java] 	at org.apache.qpid.client.BasicMessageConsumer.notifyMessage(BasicMessageConsumer.java:777)
*/
	private lazy val myServer : ServerFeatureAccess = {
		val server = new TestTAQpidServer(getAkkaSys, myQpidConnMgr)
		server
	}

	private def getServerReceiveFeature : ServerReceiveFeature = myServer.getServerRecieveFeature
	private def getServerPublishFeature : ServerPublishFeature = myServer.getServerPublishFeature

	def setUnifiedListenTeller(tellerLikesBin : CPStrongTeller[VWRqTAWrapper]) : Unit = {
		getServerReceiveFeature.setUnifiedListenTeller(tellerLikesBin)
	}

	def getVWPubNoticeSender : VWNoticeSender = getServerPublishFeature.getVWPubNoticeSender


	def checkServerSvcs() : Unit = {
		val noticeSender = getVWPubNoticeSender
		info2("My Qpid server: {}, noticeSender={}", myServer, noticeSender)
		noticeSender.sendPingNotice("NavUiApp testing VWNotice send")
	}
}

trait OffersVWorldClient extends OffersQpidSomething {
	protected lazy val myClient : TestTAQPidClient = {
		val clientDestMgr : QpidDestMgr = new QPidDestMgrJFlux(myQpidConnMgr)
		val client = new TestTAQPidClient(clientDestMgr)
		client
	}

	def checkClient() : Unit = {
		info1("Beginning checkClient for offer={}", this)
		val destMgr = myClient.getDestMgr
		info3("Finished checkClient for offer={}, client={}, destMgr={}", this, myClient, destMgr)
	}
}
