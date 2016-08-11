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

package org.friendularity.thact

import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.api.thing.ThingActionSpec

/**
  * Created by Stub22 on 8/6/2016.
  */

// To be used on client side where they may not have CPump awareness
trait ThingActSender extends VarargsLogging {
	val NO_PREFERENCE : Integer = 0
	val PREFER_JAVA_SER : Integer = 1
	val PREFER_TURTLE_SER : Integer = 2

	def supportsJavaSer : Boolean = false
	def supportsTurtleSer : Boolean = false
	def postThingAct(taSpec : ThingActionSpec, encodePref : Integer): Unit = {
		var sent = false
		if ((encodePref == PREFER_JAVA_SER) || (!supportsTurtleSer)) {
			if (supportsJavaSer) {
				postThingActViaJavaSer(taSpec)
				sent=true
			}
		}
		if (!sent) {
			if (supportsTurtleSer) {
				postThingActViaTurtleSer(taSpec)
				sent=true
			}
		}
		if (!sent) {
			error0("TA-Message not sent, no serialization pathways supported")
		}
	}

	def postThingActViaJavaSer(taSpec : ThingActionSpec): Unit = ???
	def postThingActViaTurtleSer(taSpec : ThingActionSpec): Unit = ???
}
