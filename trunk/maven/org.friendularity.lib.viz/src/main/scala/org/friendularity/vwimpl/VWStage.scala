package org.friendularity.vwimpl

import akka.actor.Actor
import org.friendularity.rbody.DualBodyRecord
import org.friendularity.vwmsg.{VWStageRqMsg, VWBodyRq}

/**
  * Created by StuB22 on 6/21/2016.
  *
  * Manages cameras, viewports and lights
  */
trait VWStageLogic {

}
class VWStageActor(someThing : AnyRef) extends Actor with VWStageLogic {

	def receive = {
		case vwsrq: VWStageRqMsg => {
			// processBodyRq(vwbrq, self, context)
		}
	}
}