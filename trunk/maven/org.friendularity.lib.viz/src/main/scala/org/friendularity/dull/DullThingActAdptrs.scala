package org.friendularity.dull

import org.appdapter.core.name.Ident
import org.cogchar.api.thing.WantsThingAction
import org.cogchar.impl.thing.route.BasicThingActionRouter
import org.friendularity.cpump.{TACPFilterAdptr, CPMsgTeller, CPRepliableThingActionMsg, CPThingActionMsg}

/**
  * Created by Owner on 4/13/2016.
  */
case class DullTACPFilterAdptrOne(taRouter : WantsThingAction)
			extends DullFilterAdptr[CPThingActionMsg] with TACPFilterAdptr[DullPumpCtx] {
	override def getFilterMsgClz = classOf[CPThingActionMsg]

	override protected def attemptShortcut(inMsg: CPThingActionMsg,
										   pumpCtx_opt: Option[DullPumpCtx]): Traversable[CPThingActionMsg] = {
		val singleOutMsg_Opt : Option[CPThingActionMsg] = inMsg match {
			// Replying here is a sloppy concept, not really implemented, more of a type-note
			case rplbl : CPRepliableThingActionMsg => {
				val rplyTeller_opt : Option[CPMsgTeller] = rplbl.getReplyTeller_opt
				routeOnewayInbound(rplbl)
				None

			}
			case ota: CPThingActionMsg => {
				routeOnewayInbound(ota)
				None
			}
		}
		singleOutMsg_Opt
	}

	def routeOnewayInbound(cprtam : CPThingActionMsg) : Unit = {
		val ta = cprtam.getThingAction
		val routerInternalMap : java.util.Map[Ident, java.util.List[WantsThingAction]] =
			taRouter.asInstanceOf[BasicThingActionRouter].hackExposeConsumerMap
		info1("routerInternalMap={}", routerInternalMap)

		// taRouter.consumeAction(ta)
	}

	def routeInboundAndCollectAnswersNow(inCprtam : CPThingActionMsg) : Traversable[CPThingActionMsg] = {
		val ta = inCprtam.getThingAction
		// taRouter.consumeAction(ta)
		Nil
	}
}