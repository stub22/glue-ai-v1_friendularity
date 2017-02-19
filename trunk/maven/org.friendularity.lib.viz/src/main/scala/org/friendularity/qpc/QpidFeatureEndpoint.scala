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

/*
import java.io.{Serializable => JSerializable}
import java.lang.{Long => JLong}
import javax.jms.{Destination => JMSDestination, Message => JMSMsg, MessageConsumer => JMSMsgConsumer,
		MessageListener => JMSMsgListener, MessageProducer => JMSMsgProducer, ObjectMessage => JMSObjMsg,
		Session => JMSSession, TextMessage => JMSTextMsg}
*/

// import akka.actor.ActorRefFactory
import org.appdapter.core.name.{Ident, FreeIdent}
import org.appdapter.fancy.log.VarargsLogging
// import org.cogchar.render.rendtest.GoodyTestMsgMaker
import org.friendularity.akact.{KnowsAkkaSys, DummyActorMaker}
import org.friendularity.cpmsg.{CPStrongTeller, ActorRefCPMsgTeller}
// import org.friendularity.vw.impl.ta.{TestTAQpidServer, ServerFeatureAccess, ServerReceiveFeature, ServerPublishFeature}

// import org.friendularity.netcli.vwta.QPidTATestClient
import org.friendularity.thact.{ThingActSender, ThingActReceiverBinary, ThingActReceiverTxt}


/**
  * Created by Stub22 on 8/8/2016.
  */

class QPidFeatureEndpoint(myJmsDestMgr : JmsDestMgr) extends  KnowsDestMgr {

	override def getJmsDestMgr : JmsDestMgr = myJmsDestMgr
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



