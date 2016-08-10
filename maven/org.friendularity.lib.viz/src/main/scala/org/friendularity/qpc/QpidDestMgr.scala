package org.friendularity.qpc


import java.io.{Serializable => JSerializable}
import java.lang.{Long => JLong}
import javax.jms.{Destination => JMSDestination, Message => JMSMsg, MessageConsumer => JMSMsgConsumer,
		MessageListener => JMSMsgListener, MessageProducer => JMSMsgProducer, ObjectMessage => JMSObjMsg,
		Session => JMSSession, TextMessage => JMSTextMsg}

import org.appdapter.fancy.log.VarargsLogging

/**
  * Created by Stub22 on 8/8/2016.
  */
trait QpidDestMgr {

	val QUEUE_PROPS_SUFFIX = "; {create: always, node: {type: queue}}";
	val TOPIC_PROPS_SUFFIX = "; {create: always, node: {type: topic}}";

	protected def getConnMgr : QpidConnMgr

	def makeSession : JMSSession = getConnMgr.makeSessionAutoAck()

//	def getDestsByNameTail :  Map[String, JMSDestination]

	def getDestForTopicName(topicName : String) : JMSDestination

	// Here "tailBare" is relative to the rest of the destination URL, and does not include suffix, unlike "TailFull".
	def makeTopicDestination(destNameTailBare: String): JMSDestination = {
		val destNameTailFull = destNameTailBare + TOPIC_PROPS_SUFFIX
		val connMgr = getConnMgr
		val dest : JMSDestination = connMgr.makeFullySpecifiedDestination(destNameTailFull)
		dest
	}

	def makeQueueDestination(destNameTailBare : String) : JMSDestination = {
		val destNameTailFull = destNameTailBare + QUEUE_PROPS_SUFFIX
		val connMgr = getConnMgr
		val dest : JMSDestination = connMgr.makeFullySpecifiedDestination(destNameTailFull)
		dest
	}
}
abstract class DestMgrImpl(myConnMgr : QpidConnMgr) extends QpidDestMgr {
	override protected def getConnMgr: QpidConnMgr = myConnMgr

	override def getDestForTopicName(topicName: String): JMSDestination = ???
}
class QPidDestMgrJFlux(connMgr : QpidConnMgr) extends DestMgrImpl(connMgr) with VarargsLogging {

	override def getDestForTopicName(topicName: String): JMSDestination = {
		makeTopicDestination(topicName)
	}
}

// Qpid example code often uses this Jndi approach.
// Here our known topic names are passed in to constructor, and used to set up JNDI properties,
// before the JmsConnection is even instantiated.
// Notes:  We have not made JNDI work under OSGi.  (Would need to resolve some class-wiring issues).
// So instead we now use the Qpid...JFlux  classes instead.
// This impl needs retesting with latest "fullySpecified" destination names.  Not sure where those fit in JNDI concept.
class QPidDestMgr_JNDI_032(myTopicExchangeNameList : List[String]) extends QpidDestMgr
			with VarargsLogging {
	private lazy val myNameMgr = new QPid_032_NameManager()

	private lazy val myJndiProps = myNameMgr.makeJndiPropsForTopicSetup(myTopicExchangeNameList)

	private lazy val myQPidConnMgr : QpidConnMgr = {
		// Notice how in this form, the topicNames are put in Jndi props before the connection is created.
		info1("QPidConn jndiProps={}", myJndiProps)
		val qc = new QpidConnMgrJndi(myJndiProps)
		qc.startConn
		qc
	}

	override protected def getConnMgr : QpidConnMgr = myQPidConnMgr

	private lazy val myDestsByNameTail : Map[String, JMSDestination] = {
		myTopicExchangeNameList.map(n => n -> myQPidConnMgr.makeFullySpecifiedDestination(n)).toMap
	}

	override def getDestForTopicName(topicName : String) : JMSDestination = {
		myDestsByNameTail.get(topicName).get
	}

}

