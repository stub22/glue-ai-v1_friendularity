package org.friendularity.netcli.bigtest

import akka.actor.{Actor, ActorLogging, ActorSystem, Address, AddressFromURIString, Props}
import org.appdapter.fancy.log.VarargsLogging
import org.friendularity.infra.dull.TestDullServer
import org.friendularity.infra.cpmsg.{ActorRefCPMsgTeller, ActorSelCPMsgTeller, RepliableTxtSymMsg, TxtSymMsg}
import org.friendularity.navui.NavUiTestPublicNames
import org.friendularity.thact.{CPTAWrapMsg, DummyThingActionMaker}

/**
  * Goal here is to post ThingActions over akka-remote so they are seen in TestCPumpServer process,
  * and get some answers back, too!
  */

object TestCPumpClientAkka extends VarargsLogging {
	def main(args: Array[String]): Unit = {

		info0 ("^^^^^^^^^^^^^^^^^^^^^^^^  TestCPumpClientAkka main().START");

		val unitTestWithoutOSGi : Boolean = false
		val serverIsNavUI : Boolean = true
		val unitSysName = if (serverIsNavUI) NavUiTestPublicNames.akkaSysName else TestDullServer.akkaSysName
		val unitPumpName = if (serverIsNavUI) NavUiTestPublicNames.cpumpName else TestDullServer.standPumpTestCtxName
		val unitSrvPort = 4719
		val unitTestServPath = "akka.tcp://" + unitSysName + "@127.0.0.1:" + unitSrvPort + "/user/" + unitPumpName
		val ccmioServName = "ccmioBundle" // Matches what is in the bundle code
		val ccmioPumpName = "ccmioPump"
		val ccmioServHost = "127.0.0.1" // Local machine
		val ccmioServPort = 4777 // matches ccmio's  application.conf
		val ccmioCpumpActPath = "/user/" + ccmioPumpName
		val ccmioOSGiServCPumpPath = "akka.tcp://" + ccmioServName + "@" + ccmioServHost + ":" + ccmioServPort + ccmioCpumpActPath;
		val betterPathAddr = new Address("akka.tcp", ccmioServName, ccmioServHost, ccmioServPort);
		val bpout = betterPathAddr.toString

		info2("homey={}     while      BetterPathOut={}", ccmioOSGiServCPumpPath, bpout);
		val serverPumpPath = if(unitTestWithoutOSGi) unitTestServPath else ccmioOSGiServCPumpPath

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
		info2("Sending rq4RespMsg={} to tellser={}", requestForResponseMsg, selTeller)
		selTeller.tellCPMsg(requestForResponseMsg)

		val taflow = new DummyThingActionMaker{}
		val ta = taflow.makeThingActionSpec()
		val tamsg = new CPTAWrapMsg(ta, Some(respConsTeller))

		info2("Sending tamsg={} to  tellser={}", tamsg, selTeller)
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
