package org.friendularity.cpump

import org.appdapter.core.name.Ident
import org.cogchar.api.thing.WantsThingAction
import org.cogchar.impl.thing.route.BasicThingActionRouter

/**
  * Created by Owner on 4/7/2016.
  */
trait TACPChanListen[CtxType <: CPumpCtx] extends CPChanListen[CPThingActionMsg, CtxType] {

}

trait TACPChanPost[CtxType <: CPumpCtx] extends CPChanPost[CPThingActionMsg, CtxType] {

}

trait TACPFilterAdptr[CtxType <: CPumpCtx] extends CPumpAdptr[CPThingActionMsg, CtxType, CPThingActionMsg] {

}

case class DullTACPFilterAdptrOne(taRouter : WantsThingAction)
			extends DullFilterAdptr[CPThingActionMsg] with TACPFilterAdptr[DullPumpCtx] {
	override def getFilterMsgClz = classOf[CPThingActionMsg]

	override protected def attemptShortcut(inMsg: CPThingActionMsg,
										   pumpCtx_opt: Option[DullPumpCtx]): Traversable[CPThingActionMsg] = {
		val singleOutMsg_Opt : Option[CPThingActionMsg] = inMsg match {
			// Replying here is a sloppy concept, not really implemented, more of a type-note
			case rplbl : CPReliableThingActionMsg => {
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