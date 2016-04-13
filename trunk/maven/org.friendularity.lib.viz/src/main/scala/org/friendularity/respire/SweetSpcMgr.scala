package org.friendularity.respire

import akka.actor.{ActorLogging, Actor}
import org.friendularity.cpump.{Greeting, WhoToGreet}

/**
  * Created by Owner on 4/13/2016.
  */
trait SweetSpcMgr {

}
trait CalcTickMsg {
}
trait ReconfMsg {
}
trait UserInputMsg {
	// Could be a screen click/touch, or other spatial or continuous input.
	// Usually not keystrokes, through this API, as those are more likely to be handled by a central controller,
	// possibly showing up here eventually as ReconfMsg commands.
}
trait StreamFrameMsg {
	// One frame of a regular continuous stream of data
}
trait StatusMsg {
}

abstract class SweetSpcActor extends Actor with ActorLogging {
	def receive = {
		case ctm: CalcTickMsg => {
		}
		case rcnf : ReconfMsg => {
		}
		case uim:  UserInputMsg => {
		}
		case odm: StreamFrameMsg => {
		}
		case stsm: StatusMsg => {
		}
	}
	protected def getMgr : SweetSpcMgr
}
class SweetSpcFactory {

}