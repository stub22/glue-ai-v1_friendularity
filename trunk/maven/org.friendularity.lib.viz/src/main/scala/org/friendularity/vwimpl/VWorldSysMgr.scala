/*
 *  Copyright 2016 by The Friendularity Project (www.friendularity.org).
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package org.friendularity.vwimpl

import akka.actor._
import com.hp.hpl.jena.rdf.model.{Model => JenaModel}
import org.appdapter.fancy.log.VarargsLogging
import org.appdapter.fancy.rclient.EnhancedLocalRepoClient
import org.cogchar.blob.entry.EntryHost
import org.cogchar.render.goody.basic.BasicGoodyCtx
import org.friendularity.raiz.TestRaizLoad
import org.friendularity.cpump.CPStrongTeller
import org.friendularity.rbody.DualBodyRecord
import org.friendularity.vwmsg.VWorldPublicTellers

/**
  * Created by Stub22 on 4/14/2016.
  */

// Legit state of a running VWorld system is managed by an instance of VWorldSysMgr.
trait VWorldSysMgr {
	// See VWPTRendezvous
	// def findPublicTellers : VWorldPublicTellers

//	def findGoodyCtx_opt : Option[BasicGoodyCtx] = None
}
// (hack)Strap holds any icky extra shared state that we temporarily want to pass to VWorldBossActor.
trait VWorldStrap {
//	def	getPumpCtx : DullPumpCtx
}


trait VWPTRendezvous {
	private var myListeners : List[CPStrongTeller[VWorldPublicTellers]] = Nil
	private var myVWPT_opt : Option[VWorldPublicTellers] = None

	def addVWPTListener (teller: CPStrongTeller[VWorldPublicTellers]) : Unit = {
		synchronized{
			myListeners = teller :: myListeners
			if (myVWPT_opt.isDefined) {
				teller.tellStrongCPMsg(myVWPT_opt.get)
			}
		}
	}
	protected def notifyVWPTListeners(vwpt : VWorldPublicTellers) : Unit = {
		synchronized {
			myListeners.foreach (_.tellStrongCPMsg (vwpt) )
		}
	}
	def setVWPT(vwpt : VWorldPublicTellers): Unit = {
		synchronized {
			myVWPT_opt = Option (vwpt)
			notifyVWPTListeners (vwpt)
		}
	}
}

class VWSysMgrImpl extends VWorldSysMgr {

	// override def findPublicTellers : VWorldPublicTellers = new VWorldPublicTellers{}

}


class VWStrapImpl extends VWorldStrap {
	// Good news - the hackStrap is still empty!  Let's hope it stays that way.
}
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
	def makeVWGoodyActor(parentARF : ActorRefFactory, goodyActorName : String, goodyCtx : BasicGoodyCtx) : ActorRef = {
		val goodyActorProps = Props(classOf[VWGoodyActor], goodyCtx)
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

