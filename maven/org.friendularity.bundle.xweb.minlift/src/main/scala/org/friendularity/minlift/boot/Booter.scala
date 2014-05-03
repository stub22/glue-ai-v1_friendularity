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

package org.friendularity.minlift.boot

import net.liftweb.http.{Bootable, Html5Properties, LiftRules, Req, LiftSession}
import net.liftweb.sitemap.{Menu, SiteMap}

/** Started with "from scratch" init code section here:    http://cookbook.liftweb.net/
 *
 * @author Stu B. <www.texpedient.com>
 */

class Booter extends Bootable {
 override def boot {
	 println("********************** minlift Booter.boot says..... Boo!")

	 // 
    // where to search for subdirs incl: snippet, 
    LiftRules.addToPackages("org.friendularity.minlift")

    def sitemap(): SiteMap = SiteMap(
      Menu.i("Home") / "index"
    )
	// Cookbook does not include the actual set-the-sitemap part!
    val oneSM = sitemap()
	LiftRules.setSiteMap(oneSM)
		
    // Use HTML5 for rendering.
	// Without this it appears we get XHTML which works as long as templates, etc. are well-formed.
	// Lift 2.5 download template default.html contains a few unclosed tag errors (for "img" and "hr" tags).
  //  LiftRules.htmlProperties.default.set((r: Req) =>
  //    new Html5Properties(r.userAgent))
 
		
		// Also from Cookbok:
		LiftSession.afterSessionCreate ::=
 ( (s:LiftSession, r:Req) => println("Session created") )

LiftSession.onBeginServicing ::=
 ( (s:LiftSession, r:Req) => println("Processing request") )

LiftSession.onShutdownSession ::=
 ( (s:LiftSession) => println("Session going away") )
 }
}

/**
 o.c.b.bind.lift Booter does this:
 
 	// SiteMap; probably not really necessary at this point
	val pushyMenu = Menu("pushy") / "index" // This very different format from Lift in Action p.45
	val cogcharMenu = Menu(Loc("cogchar", ("cogchar" :: Nil) -> true, "Cogchar Interaction Internals")) // This is for the /cogchar directory and directories inside - format from Exploring Lift I think
	val sitemap = List(pushyMenu, cogcharMenu) // Just what we need for Pushy right now
	//LiftRules.setSiteMap(SiteMap(sitemap:_*)) // This is only commented out temporarily until Ticket 23 work is fully complete

	println("##################### Booter.boot    2222222222222222")
			
	LiftRules.early.append(makeUtf8)
	val myLiftAmbassador = PageCommander.getLiftAmbassador
	// Establish connection from LiftAmbassador into PageCommander
	myLiftAmbassador.setLiftMessenger(PageCommander.getMessenger)

	println("##################### Booter.boot    333333333333333333")
		
	// Is config already ready? If so, we missed it. Let's update now.
	if (myLiftAmbassador.checkConfigReady) {
	  PageCommander.initFromCogcharRDF(PageCommander.getInitialConfigId, myLiftAmbassador.getInitialConfig)
	}
	
	println("##################### Booter.boot    444444444444444444")
		
	// Add the listener for JSON speech to the dispatch table
	LiftRules.statelessDispatchTable.append(SpeechRestListener)
	
	// Have lift automatically discard session state when it is shut down
	// (Session initialization is done via "BrowserReadyIndicator" snippet instead of LiftSession.onSetupSession
	// so that Lifter knows the browser has read the default template (or another one already loaded in browser at
	// Lifter startup) and is ready to receive a page redirect to the desired template)

	LiftSession.onShutdownSession ::= ((ls:LiftSession) => PageCommander.removeSession(ls.uniqueId))
	
	println("##################### Booter.boot   9999999999999999 ")
    
 */