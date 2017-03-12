package org.friendularity.vw.impl.job

import org.friendularity.infra.mjob.{MasterFactory, MsgFactoryPair, MsgJobLogic}
import org.friendularity.vw.msg.cor.VWorldMsg

/**
  * Created by Owner on 4/19/2016.
  */


// This trait exists as a point of human recognition and doc for VWorld job logic.
// So far it has no specific features beyond what MsgJobLogic does, other than the type constraint on Msg.
// Contravariant type marker "-" shows that this type receives/uses Msg, and does not return it as value.
// This means that supertyps of VWShow (e.g. "CPumpMsg") yield subtypes of VWJL:  VWorldJobLogic[CPumpMsg] <: VWJL[VWShowMsg]
// So if we made a really useful piece of logic that worked for all msgs, we could pass that into
// a place (method arg or collection) expecting a specific piece of logic that needs to work only for VWShowMsgs.
trait VWorldJobLogic[-Msg <: VWorldMsg] extends MsgJobLogic[Msg] {
	// Our subtypes implementing processMsgUnsafe have a lot of authority, exercisable through the main
	// actor-receive context args.
	// Overriding processMsgUnsafe is the main extension point for VWorld messaging features.
	// Impl may cause messages to other actors, creation of other actors, mutation of internal state.
	// May throw exceptions.
}

/*
Unfinished sketch of an uber-general factory
 */
trait VWorldMasterFactory extends MasterFactory {

	// Meaning of list ordering not yet specified.
	def makeBaselineMsgJobPairFactories: List[MsgFactoryPair[_ <: VWorldMsg, AnyRef]] = {
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

}
