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

import net.liftweb.http.{Html5Properties, LiftRules, Req, LiftSession}
import net.liftweb.sitemap.{Menu, SiteMap}

/** Started with "from scratch" init code section here:    http://cookbook.liftweb.net/
 *
 * @author Stu B. <www.texpedient.com>
 */

class Booter extends net.liftweb.http.Bootable {
 override def boot {
	 println("********************** minlift Booter.boot says..... Boo!")

	 // 
    // where to search for subdirs incl: snippet, 
    LiftRules.addToPackages("org.friendularity.minlift")

    // Build SiteMap
    def sitemap(): SiteMap = SiteMap(
      Menu.i("Home") / "index"
    )

    // Use HTML5 for rendering.
	// Without this it appears we get XHTML which works as long as templates, etc. are well-formed.
	// Lift 2.5 download template default.html contains a few unclosed tag errors (for "img" and "hr" tags).
    LiftRules.htmlProperties.default.set((r: Req) =>
      new Html5Properties(r.userAgent))
 
		
		// Also from Cookbok:
		LiftSession.afterSessionCreate ::=
 ( (s:LiftSession, r:Req) => println("Session created") )

LiftSession.onBeginServicing ::=
 ( (s:LiftSession, r:Req) => println("Processing request") )

LiftSession.onShutdownSession ::=
 ( (s:LiftSession) => println("Session going away") )
 }
}
