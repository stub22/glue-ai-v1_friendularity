/*
 *  Copyright 2014 by The Friendularity Project (www.friendularity.org).
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

package org.friendularity.wbrst

import akka.actor.{Props, ActorSystem, ActorRef}
import org.appdapter.core.name.{Ident, FreeIdent}
import org.appdapter.fancy.log.VarargsLogging

// https://github.com/spray/spray/tree/release/1.3

object WbrstServerTest  extends VarargsLogging {
	def main(args: Array[String]) : Unit = {
		// Backup - if logging is not working, try enabling these two lines.
		// Must enable "compile" or "provided" scope for Log4J dep in order to compile this code.
		// Note that these settings can cause double-logging, if there is a log4j.properties found.
		// org.apache.log4j.BasicConfigurator.configure();
		// org.apache.log4j.Logger.getRootLogger().setLevel(org.apache.log4j.Level.ALL);
		
		info0("^^^^^^^^^^^^^^^^^^^^^^^^  WbrstServerTest main().START");
		val wbrstAkkaSysName = "wbrstSys"
		val wbrstAkkaSys = ActorSystem(wbrstAkkaSysName)
		val svcAkkaProps = Props[FirstHttpService]
		val wbrstAkkaSvcName = "wbrstSvc"
		val svcAkkaRef = wbrstAkkaSys.actorOf(svcAkkaProps, "wbrstSvc")
		val launcher = new SprayCanLauncher {}
		val wbrstPort = 8080
		launcher.launchForListener(wbrstAkkaSys,  svcAkkaRef, wbrstPort)
		info0("^^^^^^^^^^^^^^^^^^^^^^^^  WbrstServerTest main().END");
	}

}
// https://github.com/spray/spray/tree/release/1.3
import spray.routing.HttpServiceActor

class FirstHttpService extends HttpServiceActor {
	// import spray.routing._

	override def receive = runRoute {
		get {
			path("swing") {
				complete("ALONG")
			} ~ path("swang") {
				complete("ABOUT")
			} ~ path("swung") {
				complete("AROUND")
			} ~	complete("GET Request not understood")
		} ~ post {
			complete("POST Request not understood")
		} ~ put {
			complete("PUT Request not understood")
		}
	}
}

trait SprayCanLauncher {

	import akka.io.IO
	import spray.can.Http

//	val myListener: ActorRef = // ...
	def launchForListener(actorSys : ActorSystem, aListener : ActorRef, portNum : Int) : Unit = {
		implicit val system = actorSys
		IO(Http) ! Http.Bind(aListener,  interface = "localhost", port = portNum)
	}
}

