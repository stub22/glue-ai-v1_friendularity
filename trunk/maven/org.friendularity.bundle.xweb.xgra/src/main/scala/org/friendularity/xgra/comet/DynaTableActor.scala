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

package org.friendularity.xgra.comet
import net.liftweb.http.{CometActor, CometListener, S, LiftSession, RenderOut}
import net.liftweb.common.{Full, Box}
import net.liftweb.util.Schedule
import net.liftweb.http.js.JsCmds.SetHtml

import scala.xml.{NodeSeq,Text}
import java.util.Date
import java.util.concurrent.ScheduledFuture


/**
 * @author Stu B. <www.texpedient.com>
 * 
 * The lifecycle of CometActor instance is always tied to the user-browser's LiftSession
 *(TODO: Verify above stmt) 
 *
 * http://exploring.liftweb.net/master/index-4.html#toc-Subsection-4.5.5
 * Usage: <lift:comet type="ClassName" name=”optional”/>
The <lift:comet/> tag embeds a Comet actor into your page. The class of the Comet 
actor is specified by the type attribute. 
The name attribute tells Lift to create a unique instance of the Comet actor; for example, you could 
have one Comet actor for site updates and another for admin messages. The contents of the tag are used 
by the Comet actor to bind a response...As we mention in the embed tag documentation, mixing Comet with 
AJAX responses can be a bit tricky due to the embedded JavaScript that Comet uses.
 */

class DynaTableActor extends CometActor {
	override def defaultPrefix = Full("dta")   // Full(x) is like Some(x)
	// The render method returns a NodeSeq and is used to build the first response to the client.
	override def render : RenderOut = bind("time" -%> timeSpan)   // establishes binding for this element name
	val timeInitial : String = "initial msg value, sessionID=" + myLiftSessionID 
	var myTickCount : Int = 0
	
	val initialDelayMsec : Long = 10000L
	val followupDelayMsec : Long = 5000L
	// id attrib should be preserved
	def timeSpan : NodeSeq  = <span>{timeInitial}</span>
	// Note that first-tick scheduling is in the *constructor* of this actor.
	// Each followup action must be explicitly scheduled through another call to Schedule.schedule. 
	val myFirstScheduledFuture : ScheduledFuture[Unit] = Schedule.schedule(this, Tick, initialDelayMsec) 
	override def lowPriority : PartialFunction[Any, Unit] = {
		case Tick => {
				myTickCount += 1
				val updatedMsg = "Tick=" + myTickCount + ", sessionID=" + myLiftSessionID + ", Date=" + (new Date()).toString
				println("Got tick, new message:  " + updatedMsg);
				
				partialUpdate(SetHtml("kaPoozle", Text(updatedMsg))) 
				partialUpdate(SetHtml("myWinky", Text("winky-tick=" + myTickCount))) 
				// schedule an update in 10 seconds
				val schedFutInCaseWeWantIt = Schedule.schedule(this, Tick, 10000L) 
		}
	}
	val MISSING_SESSION_ID = "MISSING-SESSION-ID";
	// Here we assume that a good time to "decide" the SessionID, forever, for this CometActor instance, is the first
	// time this value is needed, which is presumed to be during request execution.  
	lazy val myLiftSessionID : String = {
		val sessionBoxForCurrentRequest : Box[LiftSession] = S.session
		sessionBoxForCurrentRequest match {
			case Full(aLiftSession) => {
					aLiftSession.uniqueId
				}
			case _ => MISSING_SESSION_ID;
		}
	}	 	
}
case object Tick


/*
 * 
 * After navigating 
 * 
 [java] Got tick, new message:  Tick=97, Date=Wed Aug 20 12:57:43 MDT 2014
 [java] Got tick, new message:  Tick=98, Date=Wed Aug 20 12:57:53 MDT 2014
 [java] Got tick, new message:  Tick=99, Date=Wed Aug 20 12:58:03 MDT 2014
 [java] Got tick, new message:  Tick=100, Date=Wed Aug 20 12:58:13 MDT 2014
 [java] 1166988  INFO [pool-6-thread-12] net.liftweb.http.SessionMaster (Logging.scala:195) info -  Session 1mo09sz32j4sw1w3zzleecdimy expired
 [java] Session going away: net.liftweb.http.LiftSession@78fd5428
 [java] 1167006  INFO [pool-7-thread-10] net.liftweb.http.CometActor (Logging.scala:195) info - The CometActor org.friendularity.xgra.comet.DynaTableActor@5f24aa56 Received Shutdown
 [java] Got tick, new message:  Tick=101, Date=Wed Aug 20 12:58:23 MDT 2014
 [java] Got tick, new message:  Tick=102, Date=Wed Aug 20 12:58:33 MDT 2014
 [java] 1191861 DEBUG [pool-7-thread-5] net.liftweb.http.CometActor (Logging.scala:174) debug - CometActor org.friendularity.xgra.comet.DynaTableActor@5f24aa56 got unexpected message Tick

 * 
 */


/*
 Full is a Box containing a value.
 *
 *http://liftweb.net/api/25/api/net/liftweb/common/Box.html
 *
 * The Box class is a container which is able to declare if it is Full (containing a single non-null value) or EmptyBox. An EmptyBox, or empty, can be the Empty singleton, Failure or ParamFailure. Failure and ParamFailure contain information about why the Box is empty including exception information, chained Failures and a String. It serves a similar purpose to the Option class from Scala standard library but adds several features:

 you can transform it to a Failure object if it is Empty (with the ?~ method)
 you can chain failure messages on Failure Boxes
 you "run" a function on your Box, with a default value: Full(1).run("zero") { (x: String, y: Int) => y.toString }
 you can "pass" a Box to a function for side effects: Full(1) $ { x: Box[Int] => println(x openOr 0) }
 */

/*  http://liftweb.net/api/25/api/net/liftweb/http/S$.html
 *  
 * An object representing the current state of the HTTP request and response. 
 * It uses the DynamicVariable construct such that each thread has its own local session info without passing a 
 * huge state construct around. The S object is initialized by LiftSession on request startup.
 */
/* Regarding the "render" method on CometActor:
 * http://main.scala-tools.org/mvnsites/liftweb-2.0/framework/scaladocs/net/liftweb/http/CometActor.html
 * It's the main method to override, to define what is rendered by the CometActor There are implicit conversions for 
 * a bunch of stuff to RenderOut (including NodeSeq) [details]
 Thus, if you don't declare the return turn to be something other than RenderOut and return something that's coersable 
 into RenderOut, the compiler "does the right thing"(tm) for you.

 ------------------------------
 Whereas in the case of a Snippet, which a CometActor is not, we have this situation:
 
	https://groups.google.com/d/msg/liftweb/sY2g63SjN9Q/KMvbJDTr_bcJ
 
 Lift looks for a method with one of the following signatures and invokes it:

	def render(ns: NodeSeq): NodeSeq
	def render: NodeSeq => NodeSeq
 
	So this works because CssSel is a subclass of NodeSeq=>NodeSeq (or Function1[NodeSeq, NodeSeq])
	def render: CssSel
 
	In the case of the ns-argument Lift passes in the markup in the template (.html file)

 */	

