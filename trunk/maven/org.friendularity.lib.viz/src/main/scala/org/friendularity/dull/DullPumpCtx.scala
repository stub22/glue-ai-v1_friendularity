package org.friendularity.dull

import akka.actor._
import org.appdapter.core.name.{FreeIdent, Ident}
import org.appdapter.fancy.log.VarargsLogging
import org.friendularity.cpump._

import scala.collection.mutable

/**
  * Created by Owner on 4/13/2016.
  */

// This Actor serves as the subsystem "boundary" for this DullPumpCtx and its enclosed chans.
// For simplest case, see "Segregated" types below.
// Nonserializable constructor arg needs to come in through special props.
class DullPumpTopActor(myCPumpCtx : DullPumpCtx) extends Actor with ActorLogging {


	def receive = {
		// Construction of any other actors used with the ctx must happen within this handler.
		// Those actors may be sent back in receiptMsgs to answerTellers embedded in the input msg.
		case adminMsg : CPAdminRequestMsg[DullPumpCtx] => adminMsg.processInCtx(myCPumpCtx, context) // , self, context)
	}

}

trait DullPumpCtx extends CPumpCtx with CPumpListChanFinder[DullPumpCtx] with VarargsLogging {

	private val myChans = new scala.collection.mutable.HashMap[Ident, CPumpChan[DullPumpCtx]] // note outer-variance

	private[dull] def getChan(chanID : Ident) : Option[CPumpChan[DullPumpCtx]] = myChans.get(chanID)

	def getDullListenChanFinder : CPumpListChanFinder[DullPumpCtx] = this

	// By default the channels have no direct akka powers.
	// Override in cases where we want to allow channels to connect directly to actors.
	def getBoundaryTellerFinder_opt : Option[BoundaryTellerFinder] = None

	protected def allListenChans : Traversable[CPChanListen[_ <: CPumpMsg, DullPumpCtx]] = {
		myChans.values.filter(c =>{ c match {
			case listenChan : CPChanListen [_, DullPumpCtx] => true // [_ <: CPumpMsg]
			case _ => false
		}}).map(_.asInstanceOf[CPChanListen[_ <: CPumpMsg, DullPumpCtx]]).toList
	}
	override def findMsgListenChans[MK <: CPumpMsg](postChan : CPChanPost[MK, DullPumpCtx], postedMsg : MK) : Traversable[CPChanListen[MK, DullPumpCtx]] = {
		val allLCs = allListenChans
		allLCs.filter(_.interestedIn(postChan, postedMsg))
		allLCs.map(_.asInstanceOf[CPChanListen[MK, DullPumpCtx]])
	}

	// The following "makeXyzChan" methods are specifically *not* actor aware.
	def makeOnewayListenChan[MK <: CPumpMsg](chanID : Ident, msgClz : Class[MK],
											 adoptrs : Traversable[CPumpAdptr[MK, DullPumpCtx, CPumpMsg]]) : BoundedCPChanListen[MK, DullPumpCtx] = {

		val listenChan = new EZListenChan[MK, DullPumpCtx, CPumpMsg](chanID, this, adoptrs, getBoundaryTellerFinder_opt)
		myChans.put(chanID, listenChan)
		// new EZListenChan[MK, _ >: DullPumpCtx, _ <: CPumpMsg](chanID, this, adoptrs)
		// EZListenChan[InMsgKind <: CPumpMsg, CtxType <: CPumpCtx, OutBound <: CPumpMsg](chanID : Ident, ctx : CtxType,
		// myAdoptrs : Traversable[CPumpAdptr[InMsgKind, _, CtxType]]
		listenChan
	}
	def makeOnewayDispatchPostChan[MK <: CPumpMsg](chanID : Ident, msgClz : Class[MK]) : DispatchPostChan[MK, DullPumpCtx] = {

		val postChan = new EZDispatchPostChan[MK, DullPumpCtx](chanID, this, getDullListenChanFinder, getBoundaryTellerFinder_opt)
		// val subOuterActor = DullCPumpActorFactory.makeDullOuterPostActor(parentAct, this, postChan)
		myChans.put(chanID, postChan)
		postChan
	}
	def makeOnewayForwardPostChan[MK <: CPumpMsg](chanID : Ident, msgClz : Class[MK], tgtTeller: CPMsgTeller) : ForwardPostChan[MK, DullPumpCtx] = {
		val postChan = new EZForwardPostChan[MK, DullPumpCtx](chanID, this, tgtTeller, getBoundaryTellerFinder_opt)
		myChans.put(chanID, postChan)
		postChan
	}

	// When a message is addressed by Ident (e.g. from a web request), we can post it directly to the context as follows.
	// True = sent successfully, as far as we know
	// False = not sent, usually because requested post chan does not exist in this ctx.
	def forwardMsgToPostChan[MK <: CPumpMsg](chanID : Ident, msgToPost : MK) : Boolean = {
		val chanOpt : Option[CPumpChan[DullPumpCtx]] = myChans.get(chanID)
		if (chanOpt.isDefined) {
			val chan = chanOpt.get
			if (chan.isInstanceOf[CPChanPost[MK, DullPumpCtx]]) {
				val postChan = chan.asInstanceOf[CPChanPost[MK, DullPumpCtx]]
				postChan.postAndForget(msgToPost)
				debug1("Successfully posted msg to channel at ID={}", chanID)
				true
			} else {
				warn3("On ID={} found chan={}, which does not support required form of post for msg={}", chanID, chan, msgToPost)
				false
			}
		} else {
			warn3("No channel found on ID={} in this ctx={}, so cannot deliver msg={}", chanID, this, msgToPost);
			false
		}
	}
}

trait KnowsDullPumpCtx {
	// This method may decide which DullPumpCtx goes with each name, but it does not make assumptions
	// about any actors those ctxs may be associated with.
	protected def findDullPumpCtx(topActorName : String) : DullPumpCtx
}

trait DullTopActorFinder extends CachingMakingTopActorFinder with KnowsActorSystem with KnowsDullPumpCtx {

	override protected def makeTopActor(topActorName : String) : ActorRef = {
		val ctx = findDullPumpCtx(topActorName)
		val actorSys = getActorSys
		makeDullTopActor(actorSys, ctx, topActorName)
	}

	// dullCtx must encapsulate everything this actor needs to know about outside itself.
	// Conversely, the top actor constructed will *usually* be the "boundary" actor for the dullCtx.
	// However if there were more than one topActor constructed for same ctx, they would be competing
	// for that "boundary" role.  Further below, in the case of SegregatedCtx, we assume the relation is one to one.
	protected def makeDullTopActor(actorSys : ActorSystem, dullCtx : DullPumpCtx, topActorName : String) : ActorRef = {
		// How to use Props for construction of ActorRefs, see pp. 70-71 of the Akka Scala PDF Doc, v2.3.14
		/*
	"Props is a conﬁguration class to specify options for the creation of actors, think of it as an immutable and thus
	freely shareable recipe for creating an actor including associated deployment information (e.g. which dispatcher
	to use...
	val props3 = Props(classOf[ActorWithArgs], "arg")
	The last line shows a possibility to pass constructor arguments regardless of the context it is being used in.
	The presence of a matching constructor is veriﬁed during construction of the Props object, resulting in an
	IllegalArgumentException if no or multiple matching constructors are found."
		 */

		val props = Props(classOf[DullPumpTopActor], dullCtx)
		val aref : ActorRef = actorSys.actorOf(props, topActorName)
		if (dullCtx.isInstanceOf[MutaBoundedCPumpCtxImpl]) {
			// Only concern is that if the Ctx is *not* segregated, then perhaps multiple actors could try
			// to share a DullPumpCtx, but then the concept of "boundary" for the Ctx becomes ill defined.
			dullCtx.asInstanceOf[MutaBoundedCPumpCtxImpl].notifyBoundaryActorRefMade(aref)
		}
		aref
	}
}


object DullUnitTestFactoryFactory {
	private val dullAkkaSysName = "dullActorSys_UnitTest01"
	// val topDullActorName = "dullCPump01"
	private lazy val myDullAkkaSys = ActorSystem(dullAkkaSysName)  // Using case-class cons

//	private lazy val myUnitTestFactory = new DullCPumpActorFactory(myDullAkkaSys)
//	def getUnitTestFactory = myUnitTestFactory
}

object OtherCrazyThing {
	def makeDullSetup: Unit = {
		val akkaSysName = "dullActorSys01"
		val testCPumpName = "dullCPump01"
		val cpumpEndListenerName = "demoCPASTerm"

		val dullAkkaSys = ActorSystem(akkaSysName)  // Using case-class cons
		val dullPumpActor = dullAkkaSys.actorOf(Props[DullPumpTopActor], testCPumpName)
	}
	lazy val myCPumpCtx = new SegregatedBoundedDullPumpCtx ()
	lazy val postChanID : Ident = new FreeIdent("http://onto.friendularity.org/testchans#postChan017");
	lazy val listenChanID : Ident = new FreeIdent("http://onto.friendularity.org/testchans#listenChanDD");
	lazy val myPostChan01 = myCPumpCtx.makeOnewayDispatchPostChan(postChanID, classOf[TxtSymMsg])

	def emulateOldDemo = {
		val adp1 = new TxtDullFilterAdptr("filter_expr_AA")
		val adp2 = new TxtDullFilterAdptr("filter_expr_BB")
		val adptrs = List[TxtDullFilterAdptr](adp1, adp2)
		val inMsgClz = classOf[TxtSymMsg]
		val listenChan = myCPumpCtx.makeOnewayListenChan(listenChanID, inMsgClz, adptrs)

	}
	def oldDemoRcvCase(msg : CPumpMsg) : Unit = {
		msg match {
			case dmsg: TxtSymMsg => myPostChan01.postAndForget(dmsg)
		}
	}
}