package org.friendularity.dull

import akka.actor.{ActorRef, Props, ActorContext}
import org.appdapter.core.name.Ident
import org.appdapter.fancy.log.VarargsLogging
import org.friendularity.cpump._

/**
  * Created by Owner on 4/10/2016.
  */
trait DullChanAdminRqMsg  extends CPAdminRequestMsg[DullPumpCtx] with VarargsLogging {
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
	override def processInCtx(ctx : DullPumpCtx, actCtx : ActorContext): Unit = {
		// NOTE:  This transmission of adoptrs is powerful, but treading on thin ice regarding the instance closure.
		// In theory such a msg can be serialized over network, but if any adoptrs make assumptions about exec env...
		val listenChan = ctx.makeOnewayListenChan(chanID, listenedMsgClz, adoptrs)

		val localChanName = chanID.getLocalName
		info1("Local listen-chan name to be used for actor={}", localChanName)
		val chanActorProps = Props(classOf[ListenChanDirectActor[LMK]], listenChan)
		val chanActor : ActorRef = actCtx.actorOf(chanActorProps, localChanName)
		listenChan.notifyOuterActorRef(chanActor)
		sendReceiptForCreatedChan(chanID, listenChan, receiptTeller)
	}
}


case class CPARM_MakeDullPostDispatchChan[PMK <: CPumpMsg](chanID : Ident, postedMsgClz : Class[PMK],
														   receiptTeller: CPReceiptTeller)
			extends DullChanAdminRqMsg {
	override def processInCtx(ctx : DullPumpCtx, actCtx : ActorContext): Unit = {
		val postChan = ctx.makeOnewayDispatchPostChan(chanID, postedMsgClz)
		sendReceiptForCreatedChan(chanID, postChan, receiptTeller)
	}
}

case class CPARM_MakeDullPostForwardChan[PMK <: CPumpMsg](chanID : Ident, postedMsgClz : Class[PMK],
														  forwardTeller: CPMsgTeller, receiptTeller: CPReceiptTeller)
			extends DullChanAdminRqMsg {

	override def processInCtx(ctx : DullPumpCtx, actCtx : ActorContext): Unit = {
		val postChan = ctx.makeOnewayForwardPostChan(chanID, postedMsgClz, forwardTeller)
		sendReceiptForCreatedChan(chanID, postChan, receiptTeller)
	}
}
case class CPARM_LookupChanTeller(chanID : Ident, answerTeller: CPMsgTeller)
			extends DullChanAdminRqMsg {
	override def processInCtx(ctx : DullPumpCtx, actCtx : ActorContext): Unit = {
		val chanOpt = ctx.getChan(chanID)
		if (chanOpt.isDefined) {
			val outerTeller_opt = chanOpt.get.getOuterTeller_opt()
			val fotm = new FoundOuterTellerMsg(chanID, outerTeller_opt)
			answerTeller.tellCPMsg(fotm)
		}
	}
}
