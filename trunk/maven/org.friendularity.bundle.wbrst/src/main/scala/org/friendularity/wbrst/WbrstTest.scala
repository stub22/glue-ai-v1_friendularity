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

import org.appdapter.core.name.{Ident, FreeIdent}
import org.appdapter.fancy.log.VarargsLogging


object WbrstServerTest  extends VarargsLogging {
	def main(args: Array[String]) : Unit = {
		// Backup - if logging is not working, try enabling these two lines.
		// Must enable "compile" or "provided" scope for Log4J dep in order to compile this code.
		// Note that these settings can cause double-logging, if there is a log4j.properties found.
		// org.apache.log4j.BasicConfigurator.configure();
		// org.apache.log4j.Logger.getRootLogger().setLevel(org.apache.log4j.Level.ALL);
		
		info0("^^^^^^^^^^^^^^^^^^^^^^^^  WbrstServerTest main().START");


		info0("^^^^^^^^^^^^^^^^^^^^^^^^  WbrstServerTest main().END");
	}

}

import spray.routing._

class FirstHttpService extends HttpServiceActor {
	def receive = runRoute {
		path("swing") {
			get {
				complete("ALONG")
			}
		}
	}
}


