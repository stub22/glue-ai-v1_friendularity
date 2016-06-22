package org.friendularity.vwimpl

import akka.actor.Actor
import org.friendularity.vwmsg.{VWShapeMaker, VWShapeCreateRq, VWStageRqMsg}

/**
  * Created by Owner on 6/22/2016.
  */
trait VWShaperLogic {
	val shapeMaker = new VWShapeMaker{}


}

class VWShaperActor(someThing : AnyRef) extends Actor with VWShaperLogic {

	def receive = {
		case vwsrq: VWShapeCreateRq => {
			// processBodyRq(vwbrq, self, context)
		}
	}
}
