package org.friendularity.respire

import akka.actor.{ActorRef, ActorContext}
import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.impl.thing.basic.BasicThingActionSpec
import org.friendularity.cpump.{CPMsgTeller, CPumpMsg}
import com.hp.hpl.jena.rdf.model.{Model => JenaModel}
import java.lang.{Long => JLong}
/**
  * Created by Owner on 4/19/2016.
  */
trait VWorldMsg extends CPumpMsg
trait VWorldRequest  extends VWorldMsg {


}
// "Heavy" delegate approach is not recommended, but illustrates an interesting sub-case.
trait VWRequestHeavy extends VWorldRequest with VarargsLogging {
	// This kind of request includes its own impl method.  This code-shipping is quite flexible and
	// can be efficient in terms of total code-cost for features.   However, it carries several negative
	// design implications.  It is recommended against in akka prog guide.
	//
	// We demonstrate it here as just one possible semi-compliant way to map msg to behavior.
	// It is crucial that this method not attempt to mutate any shared state, or state from client.
	// If we (reasonably) assume that jobLogic is protected by same invoking actor that calls us,
	// then it is safe for this message to cause private state mutation within jobLogic.

	def processInsideUnsafe(jobLogic : VWDelegatingJobLogic[_ <: VWRequestHeavy], slf : ActorRef, sndr : ActorRef, actx : ActorContext): Unit
}
trait VWorldNotice extends VWorldMsg

trait VWorldInternalNotice extends  VWorldNotice


trait VWContentRq extends VWorldRequest {
}

trait VWCoreRq extends VWorldRequest {
	// Used to talk to internal "VWCore" actor
}
trait VWSceneCoreRq extends VWCoreRq {
	// Describes a change to managed VW scene graph, to be translated (usually by VWCore actor)
	// into calls on JME render thread.
}
// Just blobs of standalone RDF models.   Not used with query/update languages, at this time.
trait RdfMsg {
	def asTurtleString : String

	def asJenaModel(flags_opt: Option[AnyRef]) : JenaModel
	// def asR2goModel : AnyRef
}
trait RdfMsgIntrp extends VarargsLogging {
	def rcvMsg(rdfMsg: RdfMsg) = {
		val rcvdTrtlTxt = rdfMsg.asTurtleString
		debug1("Received turtle-txt msg txt, len={}", rcvdTrtlTxt.length: Integer)
		val rcvdJenm = rdfMsg.asJenaModel(None)
		info1("Received and parsed msg to model of at least {} stmt-triples.", rcvdJenm.size() : JLong)

	}
}
trait VWGoodyRqRdf extends VWContentRq  with RdfMsg {
}
trait VWGoodyRqActionSpec extends VWContentRq {
	def getActionSpec : BasicThingActionSpec
}
case class VWGoodyRqBTAS(myBTAS : BasicThingActionSpec) extends  VWGoodyRqActionSpec {
	override def getActionSpec : BasicThingActionSpec = myBTAS
}


trait VWAdminRqMsg extends VWorldRequest with VarargsLogging {

}
case class VWARM_GreetFromPumpAdmin(pumpAdminTeller : CPMsgTeller) extends VWAdminRqMsg
case class VWARM_FindGoodyTeller(answerTeller: CPMsgTeller) extends VWAdminRqMsg
case class VWARM_FindPublicTellers(answerTeller: CPMsgTeller) extends VWAdminRqMsg

// Concept:  Type filtering hooha uses concrete classes.  We expect there will be a case class Msg.
class MakeItDoOne() extends VWorldRequest {
	// No contract methods are required for this relatively *light* request, but it expects server
	// side to have sufficient logic to interpret it.  Generally that server logic will need to know
	// the type name MakeItDoOne, and pass it to some kind of handler.
}

class MakeItDoOneAy() extends MakeItDoOne
class MakeItDoOneBee() extends MakeItDoOne

case class VWSetupRq_Conf extends VWorldRequest {

}
case class VWSetupRq_Lnch extends VWorldRequest {
	// Includes callback-teller hook for result pointers after successful launch
}
case class VWSetupResultsNotice(lesserIngred: LesserIngred) extends VWorldInternalNotice

abstract class HeavyRequestTwo() extends VWRequestHeavy

case class MakeItDoTwoHeavy() extends HeavyRequestTwo() {
	override def processInsideUnsafe(jobLogic : VWDelegatingJobLogic[_ <: VWRequestHeavy], slf : ActorRef, sndr : ActorRef, actx : ActorContext): Unit = {
		// In principle the jobLogic can contain state that we know how to access.
		// It also allows us to get at the sysMgr.
		val sysMgr = jobLogic.getVWorldSysMgr
		// So, basically anything can be done from inside this method, which is both good and bad.
		// Good:  It gets a new feature implemented in just one class, with no extra factory types.
		// Bad:   It has access to basically any kind of VWorld state, without any enforced narrowing,
		// although we can try to tack some on in the jobLogic and also here in the message-class hierarchy.
	}

}


