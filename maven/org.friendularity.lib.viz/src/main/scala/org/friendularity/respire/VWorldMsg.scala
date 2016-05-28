package org.friendularity.respire

import akka.actor.{ActorRef, ActorContext}
import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.impl.thing.basic.BasicThingActionSpec
import org.friendularity.cpump.{CPStrongTeller, CPMsgTeller, CPumpMsg}
import com.hp.hpl.jena.rdf.model.{Model => JenaModel}
import java.lang.{Long => JLong}
/**
  * Created by Owner on 4/19/2016.
  */
trait VWorldMsg extends CPumpMsg
trait VWorldRequest  extends VWorldMsg {


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



trait VWAdminRqMsg extends VWorldRequest with VarargsLogging {

}
case class VWARM_GreetFromPumpAdmin(pumpAdminTeller : CPMsgTeller) extends VWAdminRqMsg
case class VWARM_FindGoodyTeller(answerTeller: CPMsgTeller) extends VWAdminRqMsg

// Receiver can wait to answer until the system is sufficiently ready, e.g. until the VWorld is up.
// However, Sndr may inquire well after the VWorld is up, and then Rcvr should answer right away.
case class VWARM_FindPublicTellers(answerTeller: CPStrongTeller[VWorldPublicTellers]) extends VWAdminRqMsg

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



