package org.friendularity.respire

import akka.actor.{ActorRef, ActorContext}
import org.appdapter.core.name.Ident
import org.appdapter.fancy.log.VarargsLogging
import org.appdapter.fancy.rclient.RepoClient
import org.cogchar.api.humanoid.{HumanoidFigureConfig, FigureConfig}
import org.cogchar.bind.mio.robot.svc.ModelBlendingRobotServiceContext
import org.cogchar.blob.emit.RenderConfigEmitter
import org.cogchar.impl.thing.basic.BasicThingActionSpec
import org.cogchar.render.app.humanoid.HumanoidRenderContext
import org.friendularity.cpump.{CPStrongTeller, CPMsgTeller, CPumpMsg}
import com.hp.hpl.jena.rdf.model.{Model => JenaModel}
import java.lang.{Long => JLong}
/**
  * Created by Owner on 4/19/2016.
  */
trait VWorldMsg extends CPumpMsg
trait VWorldRequest  extends VWorldMsg

trait VWorldNotice extends VWorldMsg

trait VWorldInternalNotice extends  VWorldNotice

trait VWContentRq extends VWorldRequest

trait VWCoreRq extends VWorldRequest {
	// Used to talk to internal "VWCore" actor
}
trait VWSceneCoreRq extends VWCoreRq {
	// Describes a change to managed VW scene graph, to be translated (usually by VWCore actor)
	// into calls on JME render thread.
}


trait VWAdminRqMsg extends VWorldRequest with VarargsLogging

trait VWCharAdminRq extends VWorldRequest

case class VWCreateCharRq(dualBodyID: Ident, fullHumaCfg : HumanoidFigureConfig,
						  myMBRoboSvcCtx : ModelBlendingRobotServiceContext,
						  answerTeller : CPStrongTeller[VWBodyNotice]) extends VWCharAdminRq

// Message sent directly to a particular existing VWBody, not for creation/deletion of same.
trait VWBodyRq extends VWorldRequest

case class VWBodyMoveRq(xPos : Float, yPos : Float, zPos : Float) extends VWBodyRq


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


case class VWARM_GreetFromPumpAdmin(pumpAdminTeller : CPMsgTeller) extends VWAdminRqMsg
// case class VWARM_FindGoodyTeller(answerTeller: CPMsgTeller) extends VWAdminRqMsg

// Receiver can wait to answer until the system is sufficiently ready, e.g. until the VWorld is up.
// However, Sndr may inquire well after the VWorld is up, and then Rcvr should answer right away.
case class VWARM_FindPublicTellers(answerTeller: CPStrongTeller[VWorldPublicTellers]) extends VWAdminRqMsg

// Concept:  Type filtering hooha uses concrete classes.  We expect there will be a case class Msg.


case class VWSetupRq_Conf() extends VWorldRequest // Not being sent as of 2016-06-16

case class VWSetupRq_Lnch() extends VWorldRequest {  // Sent from NuiiApp to VWBoss, as of 2016-06-16
	// Includes callback-teller hook for result pointers after successful launch
}

case class VWSetupResultsNotice(lesserIngred: LesserIngred, bodyMgrIngred: BodyMgrIngred) extends VWorldInternalNotice

