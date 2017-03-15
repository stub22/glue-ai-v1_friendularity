package org.friendularity.vw.impl.sys

import akka.actor._
import org.friendularity.infra.cpmsg.CPStrongTeller
import org.friendularity.vw.impl.goody.VWGoodyActor
import org.friendularity.vw.msg.cor.VWContentRq

// import com.hp.hpl.jena.rdf.model.{Model => JenaModel}
import org.cogchar.render.goody.basic.BasicGoodyCtx

import org.friendularity.rbody.DualBodyRecord
import org.friendularity.vw.impl.bdy.{VWBodyActor, VWCharMgrActor, VWCharMgrCtx}
import org.friendularity.vw.impl.boss.VWorldBossActor
import org.friendularity.vw.impl.ovl.VWOverlayActor
import org.friendularity.vw.impl.shp.VWShaperActor
import org.friendularity.vw.impl.stg.VWStageActor
/**
  * Created by Owner on 1/19/2017.
  */
object VWorldActorFactoryFuncs {

	// These are for "core" actors used within the PublicTellers boundary, (thus excluding Outer+Exo actors).

	// Following akka pattern, parentARF is either an ActorSystem (root) OR ActorContext (not-root)
	def makeVWorldBoss(parentARF : ActorRefFactory, bossActorName : String) : ActorRef = {
		val vwstrap = new VWStrapImpl
		val vwsys = new VWSysMgrImpl
		val vwbossActorProps = Props(classOf[VWorldBossActor[VWorldSysMgr]], vwsys, vwstrap)
		val vwbActorRef : ActorRef = parentARF.actorOf(vwbossActorProps, bossActorName)
		vwbActorRef
	}
	def makeVWGoodyActor(parentARF : ActorRefFactory, goodyActorName : String,
						 shprTlr : CPStrongTeller[VWContentRq]) : ActorRef = {
		val goodyActorProps = Props(classOf[VWGoodyActor], shprTlr)
		val goodyActorRef : ActorRef = parentARF.actorOf(goodyActorProps, goodyActorName)
		goodyActorRef
	}
	def makeVWCharAdminActor(parentARF : ActorRefFactory, chrMgrActorName : String, charMgrCtx : VWCharMgrCtx) : ActorRef = {
		val charMgrActorProps = Props(classOf[VWCharMgrActor], charMgrCtx)
		val chrMgrActorRef : ActorRef = parentARF.actorOf(charMgrActorProps, chrMgrActorName)
		chrMgrActorRef
	}
	def makeVWBodyActor(parentARF : ActorRefFactory, bodyActorName : String, dualBodyRec : DualBodyRecord) : ActorRef = {
		val bodyActorProps = Props(classOf[VWBodyActor], dualBodyRec)
		val bodyActorRef : ActorRef = parentARF.actorOf(bodyActorProps, bodyActorName)
		bodyActorRef
	}
	def makeVWStageActor(parentARF : ActorRefFactory, stageActorName : String, someThing : AnyRef) : ActorRef = {
		val stageActorProps = Props(classOf[VWStageActor], someThing)
		val stageActorRef : ActorRef = parentARF.actorOf(stageActorProps, stageActorName)
		stageActorRef
	}

	def makeVWShaperActor(parentARF : ActorRefFactory, shaperActorName : String, someThing : AnyRef) : ActorRef = {
		val shaperActorProps = Props(classOf[VWShaperActor], someThing)
		val shaperActorRef : ActorRef = parentARF.actorOf(shaperActorProps, shaperActorName)
		shaperActorRef
	}
	def makeVWOverlayActor(parentARF : ActorRefFactory, ovlActorName : String, someThing : AnyRef) : ActorRef = {
		val ovlProps = Props(classOf[VWOverlayActor], someThing)
		val ovlActorRef : ActorRef = parentARF.actorOf(ovlProps, ovlActorName)
		ovlActorRef
	}

}
