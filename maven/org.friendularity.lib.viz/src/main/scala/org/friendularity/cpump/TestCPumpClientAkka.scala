package org.friendularity.cpump

import akka.actor._
import org.appdapter.fancy.log.VarargsLogging

/**
  * Goal here is to post ThingActions over akka-remote so they are seen in TestCPumpServer process,
  * and get some answers back, too!
  */

object TestCPumpClientAkka extends VarargsLogging {
	def main(args: Array[String]): Unit = {

		info0 ("^^^^^^^^^^^^^^^^^^^^^^^^  TestCPumpClientAkka main().START");

		// Need to override the netty port used here, yes?
		// (since we presume TestCPumpServer is using the one from o.f.lib.viz / application.conf)

		val unitTestServPath = "akka.tcp://demoCPASys01@127.0.0.1:4719/user/demoCPump01"
		val ccmioOSGiServPath = "akka.tcp://bundle-135-ActorSystem@127.0.0.1:4777/user/demoCPump01"
		val serverPumpPath = ccmioOSGiServPath

		val akkaSysName = "clientCPASys44"

		val ourServerPort_asTxt = "4924"
		val serverPortProp = "akka.remote.netty.tcp.port"
		System.setProperty(serverPortProp, ourServerPort_asTxt)
		val clientAkkaSys = ActorSystem(akkaSysName)
		val remotePumpSel = clientAkkaSys.actorSelection(serverPumpPath)
		info1("Made remotePumpSel: {}", remotePumpSel)

		val selTeller = new ActorSelCPMsgTeller(remotePumpSel)

		val clientMsg03 = new TxtSymMsg("Sweet message from remote client")

		selTeller.tellCPMsg(clientMsg03)

		val respConsActorRef = clientAkkaSys.actorOf(Props[ResponseConsumer], "cliRespCons")
		val respConsTeller = new ActorRefCPMsgTeller(respConsActorRef)
		val requestForResponseMsg = new RepliableTxtSymMsg ("Please call me back", Some(respConsTeller))

		selTeller.tellCPMsg(requestForResponseMsg)

		import org.friendularity.ignore.nexjen.ThingActionFlow
		val taflow = new ThingActionFlow
		val ta = taflow.makeThingActionSpec()
		val tamsg = new CPTAWrapMsg(ta, Some(respConsTeller))

		selTeller.tellCPMsg(tamsg)

		val one = AddressFromURIString("akka.tcp://sys@host:1234")
		val two = Address("akka.tcp", "sys", "host", 1234) // this gives the same
		info0 ("^^^^^^^^^^^^^^^^^^^^^^^^  TestCPumpClientAkka main().END");
	}
}
class ResponseConsumer  extends Actor with ActorLogging {
	def receive = {
		case rmsg: TxtSymMsg => {
			log.info("Received response: {}", rmsg)
		}
	}
}
