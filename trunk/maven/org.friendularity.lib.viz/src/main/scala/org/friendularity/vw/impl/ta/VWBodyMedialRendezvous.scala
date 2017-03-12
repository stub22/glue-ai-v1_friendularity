package org.friendularity.vw.impl.ta

import akka.actor.ActorRef
import org.appdapter.core.name.Ident
import org.appdapter.fancy.log.VarargsLogging
import org.friendularity.infra.cpmsg.{ActorRefCPMsgTeller, CPStrongTeller}
import org.friendularity.vw.msg.bdy.{VWBodyFindRq, VWBodyNotice, VWBodyRq, VWBodyLifeRq}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
  * Created by Owner on 1/26/2017.
  */
trait VWBodyMedialRendezvous extends VarargsLogging {
	def getCharAdminTeller : CPStrongTeller[VWBodyLifeRq]

	val myBodyTlrsByID = new mutable.HashMap[Ident, CPStrongTeller[VWBodyRq]]
	val myFailedBodyIDs = new mutable.HashSet[Ident]
	val myPendingBodyQueues = new mutable.HashMap[Ident,ListBuffer[VWBodyRq]]
	def noticeBody(bn : VWBodyNotice) : Unit = {
		val bodyID : Ident = bn.getVWBodyID
		val btlr_opt : Option[CPStrongTeller[VWBodyRq]] = bn.getBodyTeller_opt
		if (btlr_opt.isDefined) {
			val btlr = btlr_opt.get
			info2("Registering TA-router path for bodyID={}, teller={}", bodyID, btlr)
			myBodyTlrsByID.put(bodyID, btlr)
			val pendingQueue_opt = myPendingBodyQueues.get(bodyID)
			pendingQueue_opt.map(pq => {
				myPendingBodyQueues.remove(bodyID)
				info2("Delivering {} pending messages to body teller={}", pq.length : Integer, btlr)
				pq.toList.map(msg => {
					btlr.tellStrongCPMsg(msg)
				})
			})
		} else {
			myFailedBodyIDs.add(bodyID)
			val removedPQ_opt : Option[ListBuffer[VWBodyRq]] = myPendingBodyQueues.remove(bodyID)
			val len_opt : Option[Integer] = removedPQ_opt.map(_.length)
			warn2("Received empty body notice for bodyID={}, discarding {} pending TA-RQs, " +
						" and adding body to permanently failed list", bodyID, len_opt)

		}
	}
	// whoDat is an existing actor who wants the notices.  We wrap in a strong-typed teller, and wrap that
	// in an interest request we send to the charAdmin teller.
	private def requestBodyNotice(bodyID : Ident, whoDat : ActorRef) : Unit = {
		val answrTeller = new ActorRefCPMsgTeller[VWBodyNotice](whoDat)
		val findBody = new VWBodyFindRq(bodyID, answrTeller)
		getCharAdminTeller.tellStrongCPMsg(findBody)
	}

	//  Peform correct action for received client msg for a body, based on whether the body is found or not.
	def routeBodyRq(bodyID : Ident, msgForBody : VWBodyRq, whoDat : ActorRef) : Unit = {
		// Resolve message body-URI to bodyActor
		if (myBodyTlrsByID.isDefinedAt(bodyID)) {
			val bodyTlr = myBodyTlrsByID.get(bodyID).get
			// Normal case, we found the bodyTlr, so let's send the VWBodyRq message there now.
			// Note that whoDat is not part of that msg, so replies to client are not possible.
			// Those are all instead sent out as notices.
			bodyTlr.tellStrongCPMsg(msgForBody)
		} else if (myFailedBodyIDs.contains(bodyID)) {
			warn1("Ignoring request sent to known-failed body with ID={}", bodyID)
		} else {
			// Since the bodyTlr was not found, we want to queue our request to be processed later, if+when
			// the bodyTlr becomes available.   We also assume that client's actor will want notices from that body.
			val pendingQueue : ListBuffer[VWBodyRq] = myPendingBodyQueues.getOrElseUpdate(bodyID, {
				info2("Requesting body notice for bodyID={} be delivered to {}", bodyID, whoDat)
				requestBodyNotice(bodyID, whoDat)
				new ListBuffer[VWBodyRq]
			})
			pendingQueue.append(msgForBody)
			info2("Appended body request to pending queue for bodyID={}, which now has len={}", bodyID, pendingQueue.length : Integer)
		}
	}
}