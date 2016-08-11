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
import org.friendularity.akact.DummyActorMaker
import org.friendularity.cpmsg.{ActorRefCPMsgTeller, CPStrongTeller}
import org.friendularity.thact.{ThingActReceiverTxt, ThingActReceiverBinary}
import org.friendularity.vwmsg.{VWGoodyRqRdf, VWGoodyRqActionSpec}

/**
  * Created by Stub22 on 8/10/2016.
  */

trait ServerPublishFeature {
	def getVWPubNoticeSender : VWNoticeSender
}
trait ServerReceiveFeature {

	def setSerBinListenTeller(tellerLikesSerBin : CPStrongTeller[VWGoodyRqActionSpec])

	def setTurtleTxtListenTeller(tellerLikesSerBin : CPStrongTeller[VWGoodyRqRdf])

	// Should be unnecessary, unless we decide to process more in RDF space with inference+onto.
	// def setTurtleTxtListenTeller (tellerLikesGoodyRdf : CPStrongTeller[VWGoodyRqRdf])
}
trait MakesRqConsumers extends KnowsVWTARqDestinations {
	private lazy val myJmsSession = getDestMgr.getJmsSession
	protected lazy val myConsumer_forTurtleTxt: JMSMsgConsumer = myJmsSession.createConsumer(destVWRqTATxt)
	protected lazy val myConsumer_forJSerBin: JMSMsgConsumer = myJmsSession.createConsumer(destVWRqTABin)
}

class ServerReceiveFeatureImpl(destMgr : QpidDestMgr) extends QPidFeatureEndpoint(destMgr)
			with ServerReceiveFeature with  MakesRqConsumers {

	/* Oops, we need to watch out for:
	 Exception=javax.jms.IllegalStateException: Attempt to alter listener while session is started.
	 caught in logic=org.friendularity.navui.OuterJobbyWrapper@6f52d3a9 during process of msg=VWPubTellersMsgImpl
	 */
	override def setSerBinListenTeller(tellerLikesSerBin : CPStrongTeller[VWGoodyRqActionSpec]) : Unit = {
		val taRcvrBin = new ThingActReceiverBinary(tellerLikesSerBin)
		val listener : JMSMsgListener = taRcvrBin.makeListener
		myConsumer_forJSerBin.setMessageListener(listener)
	}

	override def setTurtleTxtListenTeller(tellerLikesTrtlTxt : CPStrongTeller[VWGoodyRqRdf]) : Unit = {
		val taRcvrTxt : ThingActReceiverTxt = new ThingActReceiverTxt(tellerLikesTrtlTxt)
		val listener : JMSMsgListener = taRcvrTxt.makeListener
		myConsumer_forTurtleTxt.setMessageListener(listener)
	}

}
class ServerPublishFeatureImpl(destMgr : QpidDestMgr) extends QPidFeatureEndpoint(destMgr)
			with ServerPublishFeature with  KnowsVWPubStatDestinations {
	private lazy val myJmsSession = getDestMgr.getJmsSession
	private lazy val myJmsProdForVWPubNoticeBin : JMSMsgProducer = myJmsSession.createProducer(destForVWPubStatsBin)
	private lazy val mySenderForVWPubNoticeBin :  VWNoticeSender = new VWNoticeSenderJmsImpl(myJmsSession, myJmsProdForVWPubNoticeBin)

	override def getVWPubNoticeSender : VWNoticeSender = mySenderForVWPubNoticeBin
	//	def sendNotice(notice : VWorldNotice): Unit = mySender.postThingAct(taSpec, encodePref)

}

trait ServerFeatureAccess {
	def getServerRecieveFeature : ServerReceiveFeature
	def getServerPublishFeature : ServerPublishFeature
}
// Receives vw-requests in any ta-format, and hands them to a dummy dumping actor by default.
// Also publishes vw-notices as binary only.
class TestTAQpidServer(myParentARF : ActorRefFactory, myQpidConnMgr: QpidConnMgr)
			extends ServerFeatureAccess with DummyActorMaker {

	lazy val rcvDestMgr = new QPidDestMgrJFlux(myQpidConnMgr)
	lazy val rcvFeatImpl = new ServerReceiveFeatureImpl(rcvDestMgr)
	override def getServerRecieveFeature : ServerReceiveFeature = rcvFeatImpl


	lazy val pubDestMgr = new QPidDestMgrJFlux(myQpidConnMgr)
	lazy  val pubFeatImpl = new ServerPublishFeatureImpl(pubDestMgr)
	override def getServerPublishFeature : ServerPublishFeature = pubFeatImpl

	def installDumpingListeners() {

		// Dummy actor+teller to receive thingActions, of both kinds.
		// TODO:  The text-to-binary route can be built in.
		val dummyRcvActor = makeTestDummyActor(myParentARF, "dummy-goody-rq-rcvr")
		val dummyRcvWeakTeller = new ActorRefCPMsgTeller(dummyRcvActor)

		// Default setup:  Make extractor wrappers, ask them to make listeners, and attach those listeners to our JmsConsumers.
		// We use our dummyActor by default, but subclass or user could call these set...Teller methods again
		// to install a more useful actor as the listener for either channel.
		rcvFeatImpl.setSerBinListenTeller(dummyRcvWeakTeller.asInstanceOf[CPStrongTeller[VWGoodyRqActionSpec]])
		rcvFeatImpl.setTurtleTxtListenTeller(dummyRcvWeakTeller.asInstanceOf[CPStrongTeller[VWGoodyRqRdf]])
	}
}