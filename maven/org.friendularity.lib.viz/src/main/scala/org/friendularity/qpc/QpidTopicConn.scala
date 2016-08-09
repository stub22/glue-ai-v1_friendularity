package org.friendularity.qpc


import java.io.{Serializable => JSerializable}
import java.lang.{Long => JLong}
import javax.jms.{Destination => JMSDestination, Message => JMSMsg, MessageConsumer => JMSMsgConsumer,
		MessageListener => JMSMsgListener, MessageProducer => JMSMsgProducer, ObjectMessage => JMSObjMsg,
		Session => JMSSession, TextMessage => JMSTextMsg}

import org.appdapter.fancy.log.VarargsLogging

/**
  * Created by Owner on 8/8/2016.
  */
trait QpidTopicConn {

	protected def getConnMgr : QpidConnMgr

	def makeSession : JMSSession = getConnMgr.makeSessionAutoAck()

//	def getDestsByNameTail :  Map[String, JMSDestination]

	def getDestForTopicName(topicName : String) : JMSDestination

}

// Topic names are passed in to constructor, and used to set up JNDI properties,
// before the JmsConnection is even instantiated.
class QPidTopicConn_JNDI_032(myTopicExchangeNameList : List[String]) extends QpidTopicConn
			with VarargsLogging {
	private lazy val myNameMgr = new QPid_032_NameManager()

	private lazy val myJndiProps = myNameMgr.makeJndiPropsForTopicSetup(myTopicExchangeNameList)

	private lazy val myQPidConnMgr : QpidConnMgr = {
		// Notice how in this form, the topicNames are put in Jndi props before the connection is created.
		info1("QPidConn jndiProps={}", myJndiProps)
		val qc = new QpidConnMgrJndi(myJndiProps)
		qc.startConn()
		qc
	}

	override protected def getConnMgr : QpidConnMgr = myQPidConnMgr

	private lazy val myDestsByNameTail : Map[String, JMSDestination] = {
		myTopicExchangeNameList.map(n => n -> myQPidConnMgr.makeDestination(n)).toMap
	}

	override def getDestForTopicName(topicName : String) : JMSDestination = {
		myDestsByNameTail.get(topicName).get
	}


	//	override def getDestsByNameTail :  Map[String, JMSDestination] = myDestsByNameTail
				// = myQpidTopicMgr.getDestsByNameTail.get(TestAppNames.topicName_forTurtleTxtTA).get
}

