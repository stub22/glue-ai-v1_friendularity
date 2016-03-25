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

package org.friendularity.cpump

import akka.actor.ActorSystem
import akka.actor.Props
import akka.actor.ActorRef
import akka.actor.Actor
import akka.actor.ActorLogging
import akka.actor.Terminated

import org.appdapter.core.log.BasicDebugger;
import org.appdapter.fancy.log.VarargsLogging;
import org.appdapter.core.name.{FreeIdent, Ident}



object DemoCPump extends VarargsLogging {
	def main(args: Array[String]) : Unit = {

		// These two lines activate Log4J (at max verbosity!) without requiring a log4j.properties file.
		// However, when a log4j.properties file is present, these commands should not be used.
	//	org.apache.log4j.BasicConfigurator.configure();
	//	org.apache.log4j.Logger.getRootLogger().setLevel(org.apache.log4j.Level.ALL);
	//
	//	Appears that currently Akka is automatically initing logging with our log4j.properties.

		info0("^^^^^^^^^^^^^^^^^^^^^^^^  DemoCPump main().START");
		val myDCPM = new DemoCPumpMgr
		val akkaSys = myDCPM.getActorSys
		info2("ActorSystem {}\nSettings dump: {}", akkaSys, akkaSys.settings)
		// val cpumpActorRef : ActorRef = myDCPM.getCPumpActRef
		// info1("^^^^^^^^^^^^^^^^^^^^^^^^  DemoCPump main() - got initial cpumpActorRef: {}", cpumpActorRef);
		myDCPM.connectCPumpActorSystemTerminator
		// Typical result dumps as   Actor[akka://demoCPAS/user/demoCPump01#618243248]

		val tsm01 = new TxtSymMsg("First contents")

		val rootTeller = myDCPM.getRootTeller
		rootTeller.tellCPMsg(tsm01)

		val tsm02 = new TxtSymMsg("Second  contents")

		rootTeller.tellCPMsg(tsm02)

		myDCPM.terminateCPumpActors

		info0("^^^^^^^^^^^^^^^^^^^^^^^^  DemoCPump main().END");
	}


	def makeDullSetup: Unit = {
		val akkaSysName = "dullActorSys01"
		val testCPumpName = "dullCPump01"
		val cpumpEndListenerName = "demoCPASTerm"

		val dullAkkaSys = ActorSystem(akkaSysName)  // Using case-class cons
		val dullPumpActor = dullAkkaSys.actorOf(Props[DullPumpTopActor], testCPumpName)
	}
}


// Wrapper for both an ActorSystem and a cpump-factory actor
class DemoCPumpMgr extends VarargsLogging {
	// typical cpumpActorRef: Actor[akka://demoCPAS/user/demoCPump01#-1369953355]
	val akkaSysName = "demoCPASys01"
	val testCPumpName = "demoCPump01"
	val cpumpEndListenerName = "demoCPASTerm"

	lazy private val myAkkaSys = ActorSystem(akkaSysName)  // Using case-class cons
	private[cpump] def getActorSys : ActorSystem = myAkkaSys

	lazy private val myCPumpActRef : ActorRef = getActorSys.actorOf(Props[DemoCPumpActor], testCPumpName)
	private def getCPumpActRef : ActorRef = myCPumpActRef

	lazy private val myRootTeller = new ActorRefCPMsgTeller(myCPumpActRef)
	def getRootTeller = myRootTeller

	def connectCPumpActorSystemTerminator : Unit = {
		val cpumpActorRef = getCPumpActRef
		val cpumpEndListener : ActorRef = getActorSys.actorOf(Props(classOf[AkkaSysTerminator], cpumpActorRef), cpumpEndListenerName)
	}

	def terminateCPumpActors : Unit = {
		val pp = akka.actor.PoisonPill
		getCPumpActRef ! pp
	}

}
case class WhoToGreet(who: String)
case class Greeting(message: String)
case class CheckGreeting // Corresponds to an object called "Greeting" in akka-HelloWorld


// This actor is used as a local factory for message-pumping actors.
// It responds to async requests
class DemoCPumpActor extends Actor with ActorLogging {

	lazy val myCPumpCtx = new DullPumpCtx ()
	val postChanID : Ident = new FreeIdent("http://onto.friendularity.org/testchans#postChan017");
	val listenChanID : Ident = new FreeIdent("http://onto.friendularity.org/testchans#listenChanDD");
	val myPostChan01 = myCPumpCtx.makeOnewayDispatchPostChan(postChanID, classOf[TxtSymMsg])
	val adp1 = new TxtDullFilterAdptr("filter_expr_AA")
	val adp2 = new TxtDullFilterAdptr("filter_expr_BB")
	val adptrs = List[TxtDullFilterAdptr](adp1, adp2)
	val inMsgClz = classOf[TxtSymMsg]
	val listenChan = myCPumpCtx.makeOnewayListenChan(listenChanID, inMsgClz, adptrs)
  	var myMutableGreetTxt = "Heyo - initial contents of myMutableGreetTxt"

  def receive = {
	  // s"txtPat"  syntax added in Scala 2.10.    http://docs.scala-lang.org/overviews/core/string-interpolation.html
    case WhoToGreet(who) => myMutableGreetTxt = s"hello, $who"
    case CheckGreeting   => sender ! Greeting(myMutableGreetTxt) // Send the current greeting back to the sender
	case dmsg: TxtSymMsg => myPostChan01.postAndForget(dmsg)
  }
}

/*
 * Last version of akka to support Java6-7 (+ Scala 2.10) was Akka 2.3.14, released Sep 2015.
 * Latest 2.4.1 now requires Java8 + Scala 11.
 * http://docs.scala-lang.org/overviews/core/actors-migration-guide.html
 * 
 * At this point user code is ready to operate on Akka actors. 
 * Now we can switch the actors library from Scala to Akka actors. 
 * To do this configure the build to exclude the scala-actors.jar and the scala-actors-migration.jar, and to include 
 * akka-actor.jar and typesafe-config.jar. 
 * The AMK is built to work only with Akka actors version 2.1 which are included in the Scala distribution 
 * and can be configured by these instructions.

 * After this change the compilation will fail due to different package names and slight differences in the API. 
 * We will have to change each imported actor from scala to Akka. Following is the non-exhaustive list of package 
 * names that need to be changed:

scala.actors._ -> akka.actor._
scala.actors.migration.ActWithStash -> akka.actor.ActorDSL._
scala.actors.migration.pattern.ask -> akka.pattern.ask
scala.actors.migration.Timeout -> akka.util.Timeout
---------------------------
The Akka actors are organized in Actor systems. Each actor that is instantiated must belong to one ActorSystem. 
To achieve this add an ActorSystem instance to each actor instantiation call as a first argument. 
The following example shows the transformation.

To achieve this transformation you need to have an actor system instantiated. The actor system is usually 
instantiated in Scala objects or configuration classes that are global to your system. For example:

val system = ActorSystem("migration-system")
Then apply the following transformation:

ActorDSL.actor(...) -> ActorDSL.actor(system)(...)
If many calls to actor use the same ActorSystem it can be passed as an implicit parameter. For example:

ActorDSL.actor(...) ->
  import project.implicitActorSystem
  ActorDSL.actor(...)
Finally, Scala programs are terminating when all the non-daemon threads and actors finish. 
With Akka the program ends when all the non-daemon threads finish and all actor systems are shut down. 
Actor systems need to be explicitly terminated before the program can exit. 
This is achieved by invoking the shutdown method on an Actor system. 


http://doc.akka.io/docs/akka/2.3.14/AkkaScala.pdf  - pg 22

  actorOf only ever creates a new actor, and it creates it as a direct child of the context
  on which this methodis invoked (which may be any actor or actor system).

  actorSelection only ever looks up existing actors when messages are delivered, i.e. does
  not create actors, or verify existence of actors when the selection is created.

  actorFor (deprecated in favor of actorSelection) only ever looks up an existing actor, i.e.
  does not create one

What is the Difference Between Actor Reference and Path?

An actor reference designates a single actor and the life-cycle of the reference matches that actor’s life-cycle; an
actor path represents a name which may or may not be inhabited by an actor and the path itself does not have a
life-cycle, it never becomes invalid. You can create an actor path without creating an actor, but you cannot create
an actor reference without creating corresponding actor.
Note: That definition does not hold for actorFor, which is one of the reasons why actorFor is deprecated
in favor of actorSelection.

...

You can create an actor, terminate it, and then create a new actor with the same actor path. The newly created
actor is a new incarnation of the actor. It is not the same actor. An actor reference to the old incarnation is not
valid for the new incarnation. Messages sent to the old actor reference will not be delivered to the new incarnation
even though they have the same path.

Equality of ActorRef match the intention that an ActorRef corresponds to the target actor incarnation. Two
actor references are compared equal when they have the same path and point to the same actor incarnation. A
reference pointing to a terminated actor does not compare equal to a reference pointing to another (re-created)
actor with the same path. Note that a restart of an actor caused by a failure still means that it is the same actor
incarnation, i.e. a restart is not visible for the consumer of the ActorRef.

If you need to keep track of actor references in a collection and do not care about the exact actor incarnation you
can use the ActorPath as key, because the identifier of the target actor is not taken into account when comparing
actor paths.

2.5.3 How are Actor References obtained?
There are two general categories to how actor references may be obtained: by creating actors or by looking them
up, where the latter functionality comes in the two flavours of creating actor references from concrete actor paths
and querying the logical actor hierarchy.

 */