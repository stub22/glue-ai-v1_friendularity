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

package org.friendularity.field

import akka.actor.{ActorRef, Props, ActorRefFactory}
import org.appdapter.core.name.Ident
import org.appdapter.fancy.log.VarargsLogging
import org.friendularity.akact.FrienduActor
import org.friendularity.cpmsg.{ActorRefCPMsgTeller, CPStrongTeller, CPMsgTeller, CPumpMsg}

import org.friendularity.vwimpl.IdentHlp

import scala.collection.mutable.{HashMap => MutableHashMap, ListBuffer}

/**
  * Created by Stub22 on 7/11/2016.
  */

trait FieldDataDistributor extends VarargsLogging {
	def findListenersForFieldDataLeaf(ifs: ItemFieldSpec) : Traversable[Function1[FieldDataLeaf, Unit]]
	def findListenersForFieldDataBag(ifs: ItemFieldSpec) : Traversable[Function1[FieldDataBag, Unit]]

	def distributeUpdateLeaf(fdl : FieldDataLeaf) : Unit = {
		val listeners = findListenersForFieldDataLeaf(fdl.getFieldAddress)
		listeners.map(_.apply(fdl))
	}

	def distributeUpdateBag(bdl: FieldDataBag) : Unit = {
		val listeners = findListenersForFieldDataBag(bdl.getFieldAddress)
		listeners.map(_.apply(bdl))
	}

	def processOneFieldUpdate(ifd : ItemFieldData) : Unit = {
		ifd match {
			case fdb: FieldDataBag => {
				distributeUpdateBag(fdb)
			}
			case fdl: FieldDataLeaf => {
				distributeUpdateLeaf(fdl)
			}
			case other => {
				warn2("Received unexpected data-chg value of type={}, valueDump={}", other.getClass, other)
			}
		}
	}
	def processFieldDataMsg (ifdm : ItemFieldDataMsg) : Unit = {
		val fdbf = ifdm.getFieldDataBreadthFirst
		fdbf.map(processOneFieldUpdate(_))
	}

}

class MutableFieldDataDistrib() extends FieldDataDistributor {
	// We use case class in the key to make it easier to ensure equality+hash work as intended.
	val myLeafCallbacksByFieldSpec = new MutableHashMap[ItemFieldSpecDirectImpl,
				List[Function1[FieldDataLeaf, Unit]]]
	val myBagCallbacksByFieldSpec = new MutableHashMap[ItemFieldSpecDirectImpl,
				List[Function1[FieldDataBag, Unit]]]


	override def findListenersForFieldDataLeaf(ifs: ItemFieldSpec): Traversable[(FieldDataLeaf) => Unit] = {
		myLeafCallbacksByFieldSpec.get(ifs.asInstanceOf[ItemFieldSpecDirectImpl]).getOrElse(Nil)
	}
	override def findListenersForFieldDataBag(ifs: ItemFieldSpec): Traversable[(FieldDataBag) => Unit] = {
		myBagCallbacksByFieldSpec.get(ifs.asInstanceOf[ItemFieldSpecDirectImpl]).getOrElse(Nil)
	}
	def addLeafDataListener(ifsdi : ItemFieldSpecDirectImpl, cfunc : Function1[FieldDataLeaf, Unit]) : Unit = {
		val oldList = myLeafCallbacksByFieldSpec.get(ifsdi).getOrElse(Nil)
		val updList = cfunc :: oldList
		myLeafCallbacksByFieldSpec.put(ifsdi, updList)
	}
	def addBagDataListener(ifsdi : ItemFieldSpecDirectImpl, cfunc : Function1[FieldDataBag, Unit]) : Unit = {
		val oldList = myBagCallbacksByFieldSpec.get(ifsdi).getOrElse(Nil)
		val updList = cfunc :: oldList
		myBagCallbacksByFieldSpec.put(ifsdi, updList)
	}
}
trait FlowControlState {
	private var myState : Boolean = false
	def processFlowCtrlMsg(dfcm : DistribFlowControlMsg) : Unit = {
		myState = dfcm.active
	}
}

trait UpstreamOrganizer extends IdentHlp {
	// We support only one upstream teller per item
	val myUpstreamTellersByItemID = new MutableHashMap[Ident,  CPStrongTeller[FieldInterestRegMsg]]
	// def getUniqueUpstreamTellers : Set[ CPStrongTeller[FieldInterestRegMsg]] = myUpstreamTellersByItemID.values.toSet

	val myRegisteredInterests = new ListBuffer[IntRegMsgImpl]
	val myUnregisteredInterests = new ListBuffer[ItemFieldSpec]

	def notifyUpstreamTellerAvail(notice : UpstreamTellerAvailabilityNotice) : Unit = {
		val upTell = notice.getUpstreamTeller
		val ids = notice.getKnownItemIDs
		for (id <- ids) {
			myUpstreamTellersByItemID.put(id, upTell)
			// TODO:  Here we could also scan the unregistered interests list, to see if any match an
			// item that we can now provide.  We could then allow the upstreamAvailable and registerInterest
			// messages to arrive in any order.
		}
	}
	protected def getDownstreamTeller : CPStrongTeller[ItemFieldDataMsg]

	def registerInterestInFields(specs: Traversable[ItemFieldSpec]): Unit = {
		val downTeller = getDownstreamTeller

		val relevantItemIDs : Set[Ident] = specs.map(_.getItemID).toSet
		for (rID <- relevantItemIDs) {
			val relvSpecs = specs.filter(_.getItemID.equals(rID))
			val upTeller_opt = myUpstreamTellersByItemID.get(rID)
			if (upTeller_opt.isDefined) {
				val regMsg = new IntRegMsgImpl(makeStampyRandyIdentAnon(), relvSpecs, downTeller, 0.5f)
				upTeller_opt.get.tellStrongCPMsg(regMsg)
				myRegisteredInterests.append(regMsg)
			} else {
				myUnregisteredInterests.appendAll(relvSpecs)
			}
		}
	}

}

// Callbacks need to get registered with the distributor at some point.
// That may happen:
// 1) Before this actor is created.  TODO:  In this case we could dynamically add upstream registrations later.
// 2) After the actor is created, through some process hidden from the actor.
// Not really kosher, but should work.
// 3) By explicit request to the actor of FieldCallbackRegMsgImpl, which carrys typical caveats about
// the state of the function closure.
class FieldDataDistributorActor(distributor : FieldDataDistributor) extends FrienduActor with UpstreamOrganizer {
	val tellerForMe = new ActorRefCPMsgTeller[ItemFieldDataMsg](self)
	getLogger.info("In FieldDataDistributor actor constructor, logic={}", distributor)
	def receive = {
		case upAvail : UpstreamTellerAvailabilityNotice => {
			notifyUpstreamTellerAvail(upAvail)
		}

		case fieldCbackReg : FieldCallbackRegMsgImpl  => { // Not fully serializable in-process
			// Registration requires setup on both the upstream and downstream sides.
			// First we want to collect all the relevant field-specs into one bunch for upstream reg.
			val leafRegPairs = fieldCbackReg.leafRegPairs
			val leafFieldSpecs : Traversable[ItemFieldSpec] = leafRegPairs.map(_._1)
			val bagRegPairs = fieldCbackReg.bagRegPairs
			val bagFieldSpecs : Traversable[ItemFieldSpec] = bagRegPairs.map(_._1)
			val allFieldSpecs : Traversable[ItemFieldSpec] = leafFieldSpecs ++ bagFieldSpecs
			registerInterestInFields(allFieldSpecs)
			// Next we want to register the callbacks with the downstream side, which is simply the
			// distributor bundled into this actor.  Note all the typical async-actor caveats about
			// the nature of the callback funcs.  Basically they should not depend on mutable state.
			val mutaDist = distributor.asInstanceOf[MutableFieldDataDistrib]
			leafRegPairs.map(pair => {
				val ifsdi = pair._1.asInstanceOf[ItemFieldSpecDirectImpl]
				val func = pair._2
				mutaDist.addLeafDataListener(ifsdi, func)
			})
		}
		case ifdm : ItemFieldDataMsg => {
			distributor.processFieldDataMsg(ifdm)
		}

		case msg: AnyRef => {
			val msgDump = msg.toString()
			getLogger.info("TATDA received msg of clazz={} and dump-len={}", msg.getClass, msgDump.length)
			getLogger.debug("Received message dump:\n{}", msgDump)
		}
	}

	override protected def getDownstreamTeller: CPStrongTeller[ItemFieldDataMsg] = tellerForMe
}

object FieldActorFactory {
	def makeDistribIndep(parentARF : ActorRefFactory, distroActorName : String) : CPMsgTeller = {
		val distrib = new MutableFieldDataDistrib
		val distroActorProps = Props(classOf[FieldDataDistributorActor], distrib)
		val distroActorRef : ActorRef = parentARF.actorOf(distroActorProps, distroActorName)
		val teller = new ActorRefCPMsgTeller[CPumpMsg](distroActorRef)
		teller
	}


}


