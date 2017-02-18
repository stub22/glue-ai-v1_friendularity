package org.friendularity.netcli.vwta

/**
  * Created by Stub22 on 9/5/2016.
  */

import java.io.{Serializable => JSerializable}
import java.lang.{Integer => JInt, Long => JLong}

import akka.actor.ActorRefFactory
import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.api.thing.ThingActionSpec
import org.cogchar.name.goody.GoodyNames
import org.friendularity.akact.DummyActorMaker
import org.friendularity.cpmsg.ActorRefCPMsgTeller

// import org.cogchar.render.rendtest.GoodyTestMsgMaker

import org.friendularity.qpc.{QPidFeatureEndpoint, JmsDestMgr}
import javax.jms.{Destination => JMSDestination, Message => JMSMsg, MessageConsumer => JMSMsgConsumer,
MessageListener => JMSMsgListener, MessageProducer => JMSMsgProducer, ObjectMessage => JMSObjMsg,
Session => JMSSession, TextMessage => JMSTextMsg}


import org.friendularity.thact.{TAEncodePrefs, ThingActSender}


class QPidTATestClient(qpidDestMgr : JmsDestMgr) extends QPidFeatureEndpoint(qpidDestMgr)
			with MakesVWTARqProducers with MakesVWPubStatConsumers with  ExoPubStatDumpingListenerMaker {

	val statDumpPeriod : Int = 5
	myConsumer_forVWPubStatBin.setMessageListener(makePubStatDumpingListener(statDumpPeriod))

	def sendVWRqThingAct(taSpec : ThingActionSpec, encodePref : Integer): Unit = {
		myGenSender.postThingAct(taSpec, encodePref)
	}
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

trait ClientStatListenerCouldUseAkkaButThatWouldBeWeird extends  DummyActorMaker {
	val myParentARF : ActorRefFactory = ???
	val wwStatRcvActor = makeTestDummyActor(myParentARF, "vwStatRcvr")
	val vwStatWeakTeller = new ActorRefCPMsgTeller(wwStatRcvActor)
}

