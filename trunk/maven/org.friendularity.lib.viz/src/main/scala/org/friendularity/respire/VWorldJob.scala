package org.friendularity.respire

import akka.actor._
import org.appdapter.fancy.log.VarargsLogging
import org.friendularity.cpump.CPumpMsg

/**
  * Created by Owner on 4/19/2016.
  */
// Instance of MsgJobLogic may be stateful or stateless/immutable, as defined by subtypes.
// By putting logic in here instead of directly in actor classes, we reduce the amount of
// boilerplate and actor-specific syntax used in our business logic.
trait MsgJobLogic[-Msg <: CPumpMsg] extends VarargsLogging {
	// This method has a lot of authority.
	// May cause messages to other actors, creation of other actors, mutation of internal state.
	// May throw exceptions.
	def processMsgUnsafe(msg : Msg, slf : ActorRef, sndr : ActorRef, actx : ActorContext) : Unit

	// Same as unsafe, except that exceptions are caught, logged and returned.
	def processMsgSafe(msg : Msg, slf : ActorRef, sndr : ActorRef, actx : ActorContext) : Option[Throwable] = {
		var myResultExc : Option[Throwable] = None
		try {
			processMsgUnsafe(msg, slf, sndr, actx)
		} catch {
			case thr : Throwable => {
				myResultExc = Some(thr)
				error3("Exception={} caught in logic={} during process of msg={}", thr, this, msg)
			}
		}
		myResultExc
	}

}
// All the type-matching stuff here is not strictly necessary, but we are just working to define a consistent
// pattern that gives a decent amount of type safety for both programmers and the runtime.
class FilteredMsgJobActorImpl[-Msg <: CPumpMsg](jobLogic : MsgJobLogic[Msg], msgFilterClz : Class[Msg]) extends Actor with ActorLogging {
	def receive = {
		case msg : Msg => {
			if (msgFilterClz.isInstance(msg)) {
				jobLogic.processMsgSafe(msg, self, sender, context)
			} else {
				log.error("Type matching error in actor {}, inbound msg {} is not of expected class {}",
						self, msg, msgFilterClz)
			}
		}
		case oth : Any => {
			if (msgFilterClz.isInstance(oth)) {
				log.error("REVERSE type matching error in actor {}, inbound msg {} did not match type param in case, but IS of expected class {}",
						self, oth, msgFilterClz)
			} else {
				log.warning("Job.receive for logic={} ignoring non-VWMsg {}", jobLogic, oth)
			}
		}
	}
}
// Note that type param is passed to the method, rather than affecting the trait, in this case.
// We choose this approach because the akka API itself is not really rigorously typesafe, so
// we don't get much by propagating out the message type.  In contrast the jobLogic types
// are meant to be stricter.
trait MsgJobActorMaker {
	def makeFilteredMsgJobActor[Msg <: CPumpMsg](actx : ActorContext, jobActName : String,
												   jobLogic : MsgJobLogic[Msg], msgFilterClz : Class[Msg]) : ActorRef = {
		val vwjActorProps = Props(classOf[FilteredMsgJobActorImpl[Msg]], jobLogic, msgFilterClz)
		val vwjActorRef : ActorRef = actx.actorOf(vwjActorProps, jobActName)
		vwjActorRef
	}
}
class GeneralMsgActorMakerImpl extends MsgJobActorMaker


// Note that the logicFactory itself is type-parametrized, and then the makeJobLogic method
// can do further switching at runtime based on the msgFilterClz.
// So, we could have a lot of subtypes of this Factory, or only a few subtypes, which know
// how to process more kinds of JobLogic.
trait MsgJobLogicFactory[Msg <: CPumpMsg] {
	def makeJobLogic(msgFilterClz : Class[Msg]) : MsgJobLogic[Msg]
}
// This factory knows how to make logic and actors for a  given filterClz.
trait MsgFactoryPair[Msg <: CPumpMsg]  {
	def getUpperFilterClz : Class[Msg]
	def getMsgJobLogicFactory : MsgJobLogicFactory[Msg]
	def getMsgJobActorMaker : MsgJobActorMaker

	def makeLogicAndActor (actx : ActorContext, jobActName : String, subFilterClz : Class[Msg]) : ActorRef = {
		validateSubFilterClz(subFilterClz)
		val jlf = getMsgJobLogicFactory
		// All business logic used to handle messages to the actor is encapsulated in the MsgJogLogic.
		val mjl : MsgJobLogic[Msg] = jlf.makeJobLogic(subFilterClz)
		val jam = getMsgJobActorMaker
		// Next step handles the creation of Props and then Actor.
		val jobActRef = jam.makeFilteredMsgJobActor(actx, jobActName, mjl, subFilterClz)
		jobActRef
	}
	def validateSubFilterClz(subFilterClz : Class[Msg]): Unit = {
		val upperFilterClz = getUpperFilterClz
		if (!upperFilterClz.isAssignableFrom(subFilterClz)) {
			throw new RuntimeException("Supplied subFilterClz=" + subFilterClz + " is not a subtype of my upperFilterClz=" + upperFilterClz)
		}
	}
}
// This factory pair knows how to supply a jobLogic-factory and a jobActor-maker,
// for some particular category of upperFilterClz specified in constructor param (which may
// often be ignored, as it is here by default).
class MsgJobFactoryPairImpl[Msg <: CPumpMsg](upperFilterClz : Class[Msg], jobLogicFactory : MsgJobLogicFactory[Msg],
											 jobActorMaker : MsgJobActorMaker) extends MsgFactoryPair[Msg] {
	override def getUpperFilterClz : Class[Msg] = upperFilterClz
	override def getMsgJobLogicFactory : MsgJobLogicFactory[Msg] = jobLogicFactory
	override def getMsgJobActorMaker : MsgJobActorMaker = jobActorMaker
}


trait VWorldJobLogic[-Msg <: VWorldMsg] extends MsgJobLogic[Msg] {
	// This method has a lot of authority, exercisable through the main actor-receive context args.
	// Overriding this method is the main extension point for VWorld messaging features.
	// Impl may cause messages to other actors, creation of other actors, mutation of internal state.
	// May throw exceptions.
}
trait VWDelegatingJobLogic[-Msg <: VWRequestHeavy] extends VWorldJobLogic[Msg] {
	def getVWorldSysMgr : VWorldSysMgr
}
// Using the "delegating" approach pushes logic into the received message object itself.
// This approach probably has more drawbacks than advantages..

class VWJobLogicDelegatingImpl[-Msg <: VWRequestHeavy](msgFilterClz : Class[Msg], sysMgr : VWorldSysMgr) extends VWDelegatingJobLogic[Msg] {
	override def processMsgUnsafe(msg : Msg, slf : ActorRef, sndr : ActorRef, actx : ActorContext) : Unit = {
		msg.processInsideUnsafe(this, slf, sndr, actx)
	}
	override def getVWorldSysMgr : VWorldSysMgr = sysMgr
}

// trait VWorldJobLogicFactory extends MsgJobLogicFactory {
	// override def makeJobLogic[Msg <: VWorldMsg](msgFilterClz : Class[Msg]) : MsgJobLogic[Msg] = ???
// }
class VWJLDImplFactory(sysMgr : VWorldSysMgr) extends MsgJobLogicFactory[VWRequestHeavy] {
	override def makeJobLogic(msgFilterClz : Class[VWRequestHeavy]) : MsgJobLogic[VWRequestHeavy] = {
		new VWJobLogicDelegatingImpl[VWRequestHeavy](msgFilterClz, sysMgr)
	}
}
trait VWorldMasterFactory {

	// Meaning of list ordering not yet specified.
	def makeBaselineMsgJobPairFactories: List[MsgFactoryPair[_ <: VWorldMsg]] = {
		List(
			//classOf[HeavyRequestTwo],
			/*
			makeFactoryPair(classOf[MakeItDoOne], new VWorldJobLogicFactory(){
				override def makeJobLogic[MakeItDoOne](msgFilterClz : Class[MakeItDoOne]) : MsgJobLogic[MakeItDoOne] = {
					new VWorldJobLogic[MakeItDoOne]() {
						override def processMsgUnsafe(msg : MakeItDoOne, slf : ActorRef, sndr : ActorRef, actx : ActorContext) : Unit = {

						}
					}
				}
			})
			*/
		)
	}
	def makeFactoryPair[Msg <: VWorldMsg](msgFilterClz : Class[Msg], jlFact : MsgJobLogicFactory[Msg]): MsgFactoryPair[Msg] = {
		val actMak = new GeneralMsgActorMakerImpl
		new MsgJobFactoryPairImpl(msgFilterClz, jlFact, actMak)
	}
}
// In general these logic-handler types are superior to delegation to "heavy" code-bearing messages.
// However, in this approach , besides the processMsgUnsafe impl, we must somewhere define a mapping saying
// MakeItDoOne   getsProcessedBy  SomethingDoerOne
class SomethingDoerOne extends VWorldJobLogic[MakeItDoOne] {
	override def processMsgUnsafe(msg : MakeItDoOne, slf : ActorRef, sndr : ActorRef, actx : ActorContext) : Unit = ???
}
class SomethingDoerTwo extends VWorldJobLogic[MakeItDoTwoHeavy] {
	override def processMsgUnsafe(msg : MakeItDoTwoHeavy, slf : ActorRef, sndr : ActorRef, actx : ActorContext) : Unit = ???
}

class PhonyMasterFactory extends VWorldMasterFactory {
	val factOne = new MsgJobLogicFactory[MakeItDoOne]() {
		override def makeJobLogic(msgFilterClz : Class[MakeItDoOne]) : MsgJobLogic[MakeItDoOne] = {
			new SomethingDoerOne
			//msgFilterClz match {
			//	case clzA : classOf[MakeItDoOneAy => {
			//		new VWorldJobLogic[MakeItDoOne]() {
			//
			//		}
			//	}
			// }
		}
	}
	val factPairOne = makeFactoryPair[MakeItDoOne](classOf[MakeItDoOne], factOne)
}

/*
From akka user guid.
2.2.3 Actor Best Practices
1. Actors should be like nice co-workers: do their job efﬁciently without bothering everyone else needlessly
and avoid hogging resources. Translated to programming this means to process events and generate re-
sponses (or more requests) in an event-driven manner. Actors should not block (i.e. passively wait while
occupying a Thread) on some external entity—which might be a lock, a network socket, etc.—unless it is
unavoidable; in the latter case see below.
2. Do not pass mutable objects between actors. In order to ensure that, prefer immutable messages. If the
encapsulation of actors is broken by exposing their mutable state to the outside, you are back in normal Java
concurrency land with all the drawbacks.
3. Actors are made to be containers for behavior and state, embracing this means to not routinely send behavior
within messages (which may be tempting using Scala closures). One of the risks is to accidentally share
mutable state between actors, and this violation of the actor model unfortunately breaks all the properties
which make programming in actors such a nice experience.
4. Top-level actors are the innermost part of your Error Kernel, so create them sparingly and prefer truly
hierarchical systems. This has beneﬁts with respect to fault-handling (both considering the granularity of
conﬁguration and the performance) and it also reduces the strain on the guardian actor, which is a single
point of contention if over-used.
 */