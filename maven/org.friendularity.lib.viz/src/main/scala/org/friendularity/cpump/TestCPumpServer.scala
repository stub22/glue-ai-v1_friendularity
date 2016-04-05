package org.friendularity.cpump

import akka.actor.ActorRef
import org.appdapter.fancy.log.VarargsLogging


/**
  * Offers RDF pumping services over both Akka-remoting and Spray-HTTP
  */

object TestCPumpServer extends VarargsLogging {
	def main(args: Array[String]): Unit = {

		info0("^^^^^^^^^^^^^^^^^^^^^^^^  TestCPumpServer.main()-START");
		val myDCPM = new StandaloneDemoCPumpMgr
		// val cpumpActorRef : ActorRef = myDCPM.getCPumpActRef
		// Typical result dumps as   Actor[akka://demoCPAS/user/demoCPump01#618243248]
		// info1("^^^^^^^^^^^^^^^^^^^^^^^^  TestCPumpServer. main() - got initial cpumpActorRef: {}", cpumpActorRef);
		// Establish handler so that death of cpumpActor triggers end of the ActorSystem, which
		// closes up threads and possibly allows java process to exit.
		myDCPM.connectCPumpActorSystemTerminator

		// Goal is to receive, act upon, and reply to messages sent from the TestCPumpClients,
		// using any glue.ai message pathway:  local akka, remote akka, spray HTTP, Camel QPid
		val txtMsg01 = new TxtSymMsg("Pretend input message")

		val tsm01 = new TxtSymMsg("First contents")

		val rootTeller = myDCPM.getRootTeller
		rootTeller.tellCPMsg(tsm01)

		val tsm02 = new TxtSymMsg("Second  contents")

		rootTeller.tellCPMsg(tsm02)

		// myDCPM.terminateCPumpActors

		// makeDullTopActor

		info0("^^^^^^^^^^^^^^^^^^^^^^^^  TestCPumpServer.main()-END");
	}
	def startSprayServer (portNum : Int) {

	}
}
