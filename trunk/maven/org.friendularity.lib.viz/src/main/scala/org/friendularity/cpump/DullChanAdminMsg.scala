package org.friendularity.cpump

import org.appdapter.core.name.Ident

/**
  * Created by Owner on 4/10/2016.
  */
trait DullChanAdminRqMsg  extends CPAdminRequestMsg[DullPumpCtx]

case class CPARM_MakeDullListenChan[LMK <: CPumpMsg](chanID : Ident, listenedMsgClz : Class[LMK],
													 adoptrs : Traversable[CPumpAdptr[LMK, DullPumpCtx, CPumpMsg]])
			extends DullChanAdminRqMsg {
	override def processInCtx(ctx : DullPumpCtx): Unit = {
		val listenChan = ctx.makeOnewayListenChan(chanID, listenedMsgClz, adoptrs)
	}
}


case class CPARM_MakeDullPostDispatchChan[PMK <: CPumpMsg](chanID : Ident, postedMsgClz : Class[PMK],
														   receiptTeller: CPReceiptTeller)
			extends DullChanAdminRqMsg {
	override def processInCtx(ctx : DullPumpCtx): Unit = {
		val postChan = ctx.makeOnewayDispatchPostChan(chanID, postedMsgClz)
		val receiptMsg = CreatedChanTellerMsg(chanID, postChan.getOuterTeller())
		receiptTeller.tellCPReceipt(receiptMsg)

	}
}

case class CPARM_MakeDullPostForwardChan[PMK <: CPumpMsg](chanID : Ident, postedMsgClz : Class[PMK],
														  forwardTeller: CPMsgTeller)
			extends DullChanAdminRqMsg {

	override def processInCtx(ctx : DullPumpCtx): Unit = {
		val postChan = ctx.makeOnewayForwardPostChan(chanID, postedMsgClz, forwardTeller)
		// 	val forwardingTeller : CPMsgTeller = postChan.getForwardingTeller
		//	val receiptMsg = new
	}
}
case class CPARM_LookupChanTeller(chanID : Ident, answerTeller: CPMsgTeller)
			extends DullChanAdminRqMsg {
	override def processInCtx(ctx : DullPumpCtx): Unit = {
		val chanOpt = ctx.getChan(chanID)
		if (chanOpt.isDefined) {
			val outerTeller = chanOpt.get.getOuterTeller()
			val fotm = new FoundOuterTellerMsg(chanID, outerTeller)
			answerTeller.tellCPMsg(fotm)
		}
	}
}
