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

import java.util.Properties

import org.appdapter.fancy.log.VarargsLogging

/**
  * Created by Stub22 on 8/8/2016.
  */
object QPid_010_Names {
	// These values are taken from the QPid 0.10 "hello.properties"
	// Later versions of QPid use different values.

	val qpConnFactoryKey_tail = "qpidConnectionfactory";
	val qpConnFactoryKey_full = "connectionfactory" + "." + qpConnFactoryKey_tail;
	val qpConnFactoryURL = "amqp://guest:guest@clientid/test?brokerlist='tcp://localhost:5672'"
	val jndiNamingFactory_key = "java.naming.factory.initial"
	val jndiNamingFactory_val =  "org.apache.qpid.jndi.PropertiesFileInitialContextFactory"
	val destName_key_prefix =  "destination";

	val topicExchangeDestName_value = "amq.topic";
	//// Changing the above from amq.topic yields, with QPid client 0.10 running against broker 0.12
	//	The name 'niceTopic001' supplied in the address doesn't resolve to an exchange or a queue
	/*
Caused by: org.apache.qpid.AMQException: The name 'niceTopic001' supplied in the address doesn't resolve to an exchange or a queue
at org.apache.qpid.client.AMQSession_0_10.handleAddressBasedDestination(AMQSession_0_10.java:1239)
at org.apache.qpid.client.BasicMessageProducer_0_10.declareDestination(BasicMessageProducer_0_10.java:86)
	 */
}


class QPid_010_NameManager extends VarargsLogging {
	def makeJndiPropsForTopicSetup(topicExchangeNames : List[String]) : Properties = {
		// properties.load(this.getClass().getResourceAsStream("hello.properties"));
		val jndiProps = new Properties();
		jndiProps.put(QPid_010_Names.jndiNamingFactory_key, QPid_010_Names.jndiNamingFactory_val)
		// connectionfactory.[jndiname] = [ConnectionURL]
		jndiProps.put(QPid_010_Names.qpConnFactoryKey_full, QPid_010_Names.qpConnFactoryURL);
		// "connectionfactory.qpidConnectionfactory", "amqp://guest:guest@clientid/test?brokerlist='tcp://localhost:5672'")
		for (topicExchName <- topicExchangeNames) {
			// Register an AMQP destination in JNDI
			// destination.[jniName] = [Address Format]
			val destName_full = QPid_010_Names.destName_key_prefix + "." + topicExchName
			jndiProps.put(destName_full, QPid_010_Names.topicExchangeDestName_value)
		}
		jndiProps
	}
}
object QPid_032_Names {

	val qpConnFactoryKey_tail = "qpidConnectionfactory";
	val qpConnFactoryKey_full = "connectionfactory" + "." + qpConnFactoryKey_tail;
	// Update for 0.32:  Changed to use virtual host 'default' instead of 'test'.
	// (which should give same result as omitting virutalhost).
	// Determined the name 'default' by using broker web mgmt interface.
	val useBroker026 : Boolean = true
	val useMemNode : Boolean = false
	val useFrvhn : Boolean = false
	val useRkvhn : Boolean = false

	val virtualHostName = if (useFrvhn) "friendu-vhn" else if (useRkvhn) "rkvhn" else if(useMemNode) "stu_mem_node" else if (useBroker026) "test" else "default"  // 'test' for broker v0.26, 'default' for broker v0.32
	val qpConnFactoryURL = "amqp://guest:guest@clientid/" + virtualHostName + "?brokerlist='tcp://localhost:5672'"
	val jndiNamingFactory_key = "java.naming.factory.initial"
	val jndiNamingFactory_val =  "org.apache.qpid.jndi.PropertiesFileInitialContextFactory"
	private val destName_key_prefix =  "destination";

	private val topicUrlPrefix = "amq.topic";

	def destKeyNameForTail(tail : String) = destName_key_prefix + "." + tail

	def topicUrlForTail(tail : String) = topicUrlPrefix + "/" + tail
}
class QPid_032_NameManager extends VarargsLogging {
	def makeJndiPropsForTopicSetup(topicExchangeNames : List[String]) : Properties = {
		// properties.load(this.getClass().getResourceAsStream("hello.properties"));
		val jndiProps = new Properties();
		jndiProps.put(QPid_010_Names.jndiNamingFactory_key, QPid_032_Names.jndiNamingFactory_val)
		// connectionfactory.[jndiname] = [ConnectionURL]
		jndiProps.put(QPid_010_Names.qpConnFactoryKey_full, QPid_032_Names.qpConnFactoryURL);
		// "connectionfactory.qpidConnectionfactory", "amqp://guest:guest@clientid/test?brokerlist='tcp://localhost:5672'")
		for (topicExchName <- topicExchangeNames) {
			// Register an AMQP destination in JNDI
			// destination.[jniName] = [Address Format]
			val destName_full = QPid_032_Names.destKeyNameForTail(topicExchName)
			val jndiVal : String = QPid_032_Names.topicUrlForTail(topicExchName)
			jndiProps.put(destName_full, jndiVal)
		}
		jndiProps
	}
}
