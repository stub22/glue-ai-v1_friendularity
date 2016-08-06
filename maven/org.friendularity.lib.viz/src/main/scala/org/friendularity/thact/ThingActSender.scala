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
