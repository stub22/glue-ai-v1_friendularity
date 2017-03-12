package org.friendularity.infra.dull

import akka.actor.{ActorRef, Props, ActorContext}
import org.appdapter.core.name.Ident
import org.appdapter.fancy.log.VarargsLogging
import org.friendularity.infra.cpmsg.{CPStrongTeller, CPMsgTeller, CreatedChanTellerMsg, FoundOuterTellerMsg, CPumpMsg}
import org.friendularity.infra.cpump.{CPumpChan, CPumpAdptr, ListenChanDirectActor, BoundedCPChanListen, PostChanDirectActor, BoundedCPChanPost, CPAdminRequestMsg}


/**
  * Created by Owner on 4/10/2016.
  */
trait DullChanAdminRqMsg  extends CPAdminRequestMsg[DullPumpCtx] with VarargsLogging {
	protected def sendReceiptForCreatedChan(chanID : Ident, chan : CPumpChan[DullPumpCtx],
											receiptTeller: CPStrongTeller[CreatedChanTellerMsg]): Unit = {
		val crtdOuterTeller_opt = chan.getOuterTeller_opt()
		val receiptMsg = CreatedChanTellerMsg(chanID, crtdOuterTeller_opt)
		receiptTeller.tellStrongCPMsg(receiptMsg)
	}

	protected def makePostActor[PMK <: CPumpMsg](postChan : BoundedCPChanPost[PMK, DullPumpCtx], actCtx : ActorContext): ActorRef = {
		val chanID = postChan.getChanIdent
		val localChanName = chanID.getLocalName
		info1("Local listen-chan name to be used for actor={}", localChanName)
		val chanActorProps = Props(classOf[PostChanDirectActor[PMK]], postChan)
		val chanActor : ActorRef = actCtx.actorOf(chanActorProps, localChanName)
		postChan.notifyOuterActorRef(chanActor)
		chanActor
	}
	protected def makeListenActor[LMK <: CPumpMsg](lstnChan : BoundedCPChanListen[LMK, DullPumpCtx], actCtx : ActorContext): ActorRef = {
		val chanID = lstnChan.getChanIdent
		val localChanName = chanID.getLocalName
		info1("Local listen-chan name to be used for actor={}", localChanName)
		val chanActorProps = Props(classOf[ListenChanDirectActor[LMK]], lstnChan)
		val chanActor : ActorRef = actCtx.actorOf(chanActorProps, localChanName)
		lstnChan.notifyOuterActorRef(chanActor)
		chanActor
	}
}

case class CPARM_MakeDullListenChan[LMK <: CPumpMsg](chanID : Ident, listenedMsgClz : Class[LMK],
													 adoptrs : Traversable[CPumpAdptr[LMK, DullPumpCtx, CPumpMsg]],
													 receiptTeller: CPStrongTeller[CreatedChanTellerMsg])
			extends DullChanAdminRqMsg {
	override def processInCtx(ctx : DullPumpCtx, actCtx : ActorContext): Unit = {
		// NOTE:  This transmission of adoptrs is powerful, but treading on thin ice regarding the instance closure.
		// In theory such a msg can be serialized over network, but if any adoptrs make assumptions about exec env...
		val listenChan = ctx.makeOnewayListenChan(chanID, listenedMsgClz, adoptrs)
		val lstnActor = makeListenActor(listenChan, actCtx)
		sendReceiptForCreatedChan(chanID, listenChan, receiptTeller)
	}
}


case class CPARM_MakeDullPostDispatchChan[PMK <: CPumpMsg](chanID : Ident, postedMsgClz : Class[PMK],
														   receiptTeller: CPStrongTeller[CreatedChanTellerMsg])
			extends DullChanAdminRqMsg {
	override def processInCtx(ctx : DullPumpCtx, actCtx : ActorContext): Unit = {
		val postChan = ctx.makeOnewayDispatchPostChan(chanID, postedMsgClz)
		val postActor = makePostActor(postChan, actCtx)
		sendReceiptForCreatedChan(chanID, postChan, receiptTeller)
	}
}

case class CPARM_MakeDullPostForwardChan[PMK <: CPumpMsg](chanID : Ident, postedMsgClz : Class[PMK],
														  forwardTeller: CPMsgTeller,
														  receiptTeller: CPStrongTeller[CreatedChanTellerMsg])
			extends DullChanAdminRqMsg {

	override def processInCtx(ctx : DullPumpCtx, actCtx : ActorContext): Unit = {
		val postChan = ctx.makeOnewayForwardPostChan(chanID, postedMsgClz, forwardTeller)
		makePostActor(postChan, actCtx)
		sendReceiptForCreatedChan(chanID, postChan, receiptTeller)
	}
}
case class CPARM_LookupChanTeller(chanID : Ident, answerTeller: CPStrongTeller[FoundOuterTellerMsg])
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
