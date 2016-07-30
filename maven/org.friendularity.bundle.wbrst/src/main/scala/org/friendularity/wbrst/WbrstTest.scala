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

import akka.actor.{Props, ActorSystem, ActorRef, Actor, ActorLogging}
import org.appdapter.core.name.{Ident, FreeIdent}
import org.appdapter.fancy.log.VarargsLogging


// https://github.com/spray/spray/tree/release/1.3

object WbrstServerTest  extends VarargsLogging {
	def main(args: Array[String]) : Unit = {
		// Backup - if logging is not working, try enabling these two lines.
		// Must enable "compile" or "provided" scope for Log4J dep in order to compile this code.
		// Note that these settings can cause double-logging, if there is a log4j.properties found.
		org.apache.log4j.BasicConfigurator.configure();
		org.apache.log4j.Logger.getRootLogger().setLevel(org.apache.log4j.Level.ALL);
		
		info0("^^^^^^^^^^^^^^^^^^^^^^^^  WbrstServerTest main().START");
		val wbrstAkkaSysName = "wbrstSys"
		val wbrstAkkaSys = ActorSystem(wbrstAkkaSysName)
		val routingExampleActorProps = Props[TrialRoutingHttpSvcActor]
		val wbrstRoutedActorName = "wbrstRouted"
		val routedSvcAkkaRef = wbrstAkkaSys.actorOf(routingExampleActorProps, "wbrstRoutedActorName")
		val launcher = new SprayCanLauncher {}
		val wbrstRoutedPort = 8082
		launcher.launchForListener(wbrstAkkaSys,  routedSvcAkkaRef, wbrstRoutedPort)
		info2("Finished launch routing-based service on port={}, svc={}", wbrstRoutedPort : Integer, routedSvcAkkaRef)
		val wbrstDirectPort = 8081
		val directProps = Props[TrialDirectHttpSvcActor]
		val directSvcAkkaRef = wbrstAkkaSys.actorOf(directProps, "wbrstDirect")
		launcher.launchForListener(wbrstAkkaSys,  directSvcAkkaRef, wbrstDirectPort)
		info2("Finished launch routing-based service on port={}, svc={}", wbrstDirectPort : Integer, directSvcAkkaRef)
		info0("^^^^^^^^^^^^^^^^^^^^^^^^  WbrstServerTest main().END");
	}

}
// https://github.com/spray/spray/tree/release/1.3
import spray.routing.HttpServiceActor

/*
"Actors are implemented by extending the Actor base trait and implementing the receive method.
The receive method should define a series of case statements (which has the type
PartialFunction[Any, Unit]) that defines which messages your Actor can handle, using standard
Scala pattern matching, along with the implementation of how the messages should be processed."
 */
class TrialRoutingHttpSvcActor extends spray.routing.HttpServiceActor {
	// import spray.routing._

	override def receive : PartialFunction[Any, Unit] = {
		runRoute {
			get {
				path("swing") {
					complete("ALONG")
				} ~ path("swang") {
					complete("ABOUT")
				} ~ path("swung") {
					complete("AROUND")
				} ~	noop {
					val rqstInst = requestInstance
					// GET Request not understood, rq=spray.routing.directives.BasicDirectives$$anon$3@45063171
					complete(s"GET Request not understood, rqRoute=${rqstInst}")
				}
			} ~ post {
				complete("POST Request not understood")
			} ~ put {
				complete("PUT Request not understood")
			} ~ noop {
				val rq = requestInstance
				complete("Strange request not understood")
			}
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
import spray.http.{HttpRequest, HttpResponse, HttpMethods, HttpEntity, MediaTypes, ContentType}
import spray.http.Uri
import spray.can.Http

class TrialDirectHttpSvcActor extends Actor with ActorLogging {
	lazy val indexPageResponse = HttpResponse(
		entity = HttpEntity(ContentType(MediaTypes.`text/html`),
			<html>
				<body>
					<h1>Say hello to
						<i>spray-can</i>
						!</h1>
					<p>Defined resources:</p>
					<ul>
						<li>
							<a href="/ping">/ping</a>
						</li>
						<li>
							<a href="/stream">/stream</a>
						</li>
						<li>
							<a href="/server-stats">/server-stats</a>
						</li>
						<li>
							<a href="/crash">/crash</a>
						</li>
						<li>
							<a href="/timeout">/timeout</a>
						</li>
						<li>
							<a href="/timeout/timeout">/timeout/timeout</a>
						</li>
						<li>
							<a href="/stop">/stop</a>
						</li>
					</ul>
					<p>Test file upload</p>
					<form action="/file-upload" enctype="multipart/form-data" method="post">
						<input type="file" name="datafile" multiple=""></input>
						<br/>
						<input type="submit">Submit</input>
					</form>
				</body>
			</html>.toString()
		)
	)

	override def receive  : PartialFunction[Any, Unit] = {
		// when a new connection comes in we register ourselves as the connection handler
		case connNot : Http.Connected => {
			log.info("Received Http.Connected, will now register self")
			sender() ! Http.Register(self)
		}

		case HttpRequest(HttpMethods.GET, Uri.Path("/"), _, _, _) => {
			log.info("Handling request for indexPage")
			sender() ! indexPageResponse
		}
		case HttpRequest(HttpMethods.GET, Uri.Path("/zing"), _, _, _) =>
			sender() ! HttpResponse(entity = "BANG!")

		case otherReq : HttpRequest =>
			sender() ! HttpResponse(entity = s"Ignoring your weird request: ${otherReq}")

		case otherMsg => {
			log.info("DirectSvc got some other message", otherMsg)
		}
	}
}
/*

http://spray.io/documentation/1.2.3/spray-routing/key-concepts/routes/

"In spray-routing a route is defined like this:
type Route = RequestContext => Unit
It’s a simple alias for a function taking a RequestContext as parameter."

"complete uses the given arguments to construct a Route which simply calls
requestContext.complete with the respective HttpResponse instance. Completing
the request will send the response “back up” the route structure where all
logic that wrapping directives have potentially chained into the responder
chain is run (see also The Responder Chain). Once the response hits the
top-level runRoute logic it is sent back to the underlying spray-can or
spray-servlet layer which will trigger the sending of the actual HTTP
response message back to the client."

runRoute is a method defined in trait HttpServiceBase

https://github.com/spray/spray/blob/release/1.3/spray-routing/src/main/scala/spray/routing/HttpService.scala#L30

 * Supplies the actor behavior for executing the given route.

def runRoute(route: Route)(implicit eh: ExceptionHandler, rh: RejectionHandler, ac: ActorContext,
rs: RoutingSettings, log: LoggingContext): Actor.Receive = {

		case HttpRequest(HttpMethods.GET, Uri.Path("/ping"), _, _, _) => {
			val greatResponse = HttpResponse(entity = "PONG")
			sender ! greatResponse // akka "tell"
		}
		case otherHttpReq : HttpRequest =>

		runRoute returns an Actor.Receive, and kinda wants to own the whole situation it seems.
		See method signature in comment at bottom of this file.

		All the method calls within runRoute are defined to create + compose routes.

 */