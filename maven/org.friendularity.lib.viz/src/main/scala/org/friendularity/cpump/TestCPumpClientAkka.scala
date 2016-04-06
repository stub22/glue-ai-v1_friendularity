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


		val unitTestServPath = "akka.tcp://demoCPASys01@127.0.0.1:4719/user/demoCPump01"
		val ccmioServName = "ccmioBundle" // Matches what is in the bundle code
		val ccmioServHost = "127.0.0.1" // Local machine
		val ccmioServPort = 4777 // matches ccmio's  application.conf
		val ccmioCpumpActPath = "/user/demoCPump01"
		val ccmioOSGiServCPumpPath = "akka.tcp://" + ccmioServName + "@" + ccmioServHost + ":" + ccmioServPort + ccmioCpumpActPath;
		val betterPathAddr = new Address("akka.tcp", ccmioServName, ccmioServHost, ccmioServPort);
		val bpout = betterPathAddr.toString

		info2("homey={}     while      BetterPathOut={}", ccmioOSGiServCPumpPath, bpout);
		val serverPumpPath = ccmioOSGiServCPumpPath

		val clientAkkaSysName = "clientCPASys44"

		// Client needs to override the netty port used in the default config, since we don't want to collide
		// with any running TestCPumpServer, which is using the port set in o.f.lib.viz application.conf.

		val clientPort_asTxt = "4924"
		val clientPortProp = "akka.remote.netty.tcp.port"
		System.setProperty(clientPortProp, clientPort_asTxt)
		val clientAkkaSys = ActorSystem(clientAkkaSysName)
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
