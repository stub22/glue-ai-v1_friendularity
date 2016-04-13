package org.friendularity.dull

import org.appdapter.core.name.Ident
import org.friendularity.cpump._

/**
  * Created by Owner on 4/10/2016.
  */
trait DullChanAdminRqMsg  extends CPAdminRequestMsg[DullPumpCtx] {
	protected def sendReceiptForCreatedChan(chanID : Ident, chan : CPumpChan[DullPumpCtx], receiptTeller: CPReceiptTeller): Unit = {
		val crtdOuterTeller_opt = chan.getOuterTeller_opt()
		val receiptMsg = CreatedChanTellerMsg(chanID, crtdOuterTeller_opt)
		receiptTeller.tellCPReceipt(receiptMsg)
	}
}

case class CPARM_MakeDullListenChan[LMK <: CPumpMsg](chanID : Ident, listenedMsgClz : Class[LMK],
													 adoptrs : Traversable[CPumpAdptr[LMK, DullPumpCtx, CPumpMsg]],
													 receiptTeller: CPReceiptTeller)
			extends DullChanAdminRqMsg {
	override def processInCtx(ctx : DullPumpCtx): Unit = {
		val listenChan = ctx.makeOnewayListenChan(chanID, listenedMsgClz, adoptrs)
		sendReceiptForCreatedChan(chanID, listenChan, receiptTeller)
	}
}


case class CPARM_MakeDullPostDispatchChan[PMK <: CPumpMsg](chanID : Ident, postedMsgClz : Class[PMK],
														   receiptTeller: CPReceiptTeller)
			extends DullChanAdminRqMsg {
	override def processInCtx(ctx : DullPumpCtx): Unit = {
		val postChan = ctx.makeOnewayDispatchPostChan(chanID, postedMsgClz)
		sendReceiptForCreatedChan(chanID, postChan, receiptTeller)
	}
}

case class CPARM_MakeDullPostForwardChan[PMK <: CPumpMsg](chanID : Ident, postedMsgClz : Class[PMK],
														  forwardTeller: CPMsgTeller, receiptTeller: CPReceiptTeller)
			extends DullChanAdminRqMsg {

	override def processInCtx(ctx : DullPumpCtx): Unit = {
		val postChan = ctx.makeOnewayForwardPostChan(chanID, postedMsgClz, forwardTeller)
		sendReceiptForCreatedChan(chanID, postChan, receiptTeller)
	}
}
case class CPARM_LookupChanTeller(chanID : Ident, answerTeller: CPMsgTeller)
			extends DullChanAdminRqMsg {
	override def processInCtx(ctx : DullPumpCtx): Unit = {
		val chanOpt = ctx.getChan(chanID)
		if (chanOpt.isDefined) {
			val outerTeller_opt = chanOpt.get.getOuterTeller_opt()
			val fotm = new FoundOuterTellerMsg(chanID, outerTeller_opt)
			answerTeller.tellCPMsg(fotm)
		}
	}
}
