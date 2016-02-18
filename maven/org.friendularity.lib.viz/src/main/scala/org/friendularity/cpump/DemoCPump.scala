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
import org.appdapter.core.name.{FreeIdent, Ident}

case class DummyMsg(msg : String) extends CPumpMsg {
	
}
object DemoCPump extends BasicDebugger {
	def main(args: Array[String]) : Unit = {

		// These two lines activate Log4J (at max verbosity!) without requiring a log4j.properties file.  
		// However, when a log4j.properties file is present, these commands should not be used.
		org.apache.log4j.BasicConfigurator.configure();
		org.apache.log4j.Logger.getRootLogger().setLevel(org.apache.log4j.Level.ALL);
		
		getLogger().info("^^^^^^^^^^^^^^^^^^^^^^^^  DemoCPump main().START");
		val myDCPM = new DemoCPumpMgr
		val cpumpActorRef : ActorRef = myDCPM.initSystemPumpAndTerm
		// Typical result dumps as   Actor[akka://demoCPAS/user/demoCPump01#618243248]
		getLogger().info("^^^^^^^^^^^^^^^^^^^^^^^^  DemoCPump main() - got initial cpumpActorRef: {}", cpumpActorRef);
		
		
		getLogger().info("^^^^^^^^^^^^^^^^^^^^^^^^  DemoCPump main().END");	
	}
}

class AkkaSysTerminator(ref: ActorRef) extends Actor with ActorLogging {
	// Uh, the concept here seems this Actor, when terminated, will .shutdown the rest of context actorSystem.
    context watch ref
    def receive = {
      case Terminated(_) => {
        log.info("{} has terminated, shutting down system", ref.path)
        context.system.shutdown()
	  }
    }
}


class DemoCPumpMgr {
	val akkaSysName = "demoCPAS"
	val testCPumpName = "demoCPump01"
	val cpumpEndListenerName = "demoCPASTerm"
	def initSystemPumpAndTerm : ActorRef = {
		val akkaSys01 = ActorSystem(akkaSysName)  // Using case-class cons
	    val cpumpActorRef  : ActorRef = akkaSys01.actorOf(Props[DemoCPumpActor], testCPumpName)
		val cpumpEndListener : ActorRef = akkaSys01.actorOf(Props(classOf[AkkaSysTerminator], cpumpActorRef), cpumpEndListenerName)
		cpumpActorRef 
	}
	def startMsgPump : Unit = {
		
	}
}
case class WhoToGreet(who: String)
case class Greeting(message: String)
case class CheckGreeting // This is an object called "Greeting" in HelloWorld
class DemoCPumpActor extends Actor with ActorLogging {
  var greeting = ""

  def receive = {
	  // s"   syntax added in Scala 2.10.    http://docs.scala-lang.org/overviews/core/string-interpolation.html
    case WhoToGreet(who) => greeting = s"hello, $who"
    case CheckGreeting   => sender ! Greeting(greeting) // Send the current greeting back to the sender
  }
}
// Attaches to akka ActorSystem
class MsgPumpImpl {
	
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
 */
