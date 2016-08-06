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

import org.appdapter.core.name.{FreeIdent, Ident}
import org.appdapter.fancy.log.VarargsLogging
import org.friendularity.akact.FrienduActor

import org.friendularity.vwimpl.IdentHlp

import scala.collection.mutable.{HashMap => MutableHashMap}

/**
  * Created by Stub22 on 7/11/2016.
  */
class InterestChan(initialInterestMsg : FieldInterestRegMsg) {
	private var myLastRegMsg : FieldInterestRegMsg = initialInterestMsg
	private var myLastUpdateSent_opt : Option[ItemFieldDataMsg] = None
	private var myPendingUpdateData_opt : List[ItemFieldData] = Nil

	// Return value indicates whether we published, allowing caller to count messages sent.
	def maybePublish(srcStampMsec : Long, fdbf : List[ItemFieldData]) : Boolean = {
		val currIntrstMsg = myLastRegMsg
		val relevantData = fdbf.filter(currIntrstMsg.isInterestedIn(_))
		if (relevantData.nonEmpty) {
			// TODO: In heterog environments, need to distinguish update times for varying fieldSpec keys
			// ALSO:  for this to make sense, we also need a followup timer task to sweep through and
			// make sure we have not left updates hanging.
			// It could also be more sophisticated in terms of estimating update rate, rather than just
			// time-since-last
			val lastUpdateTime_opt = myLastUpdateSent_opt.map(_.getSourceTStampMsec)
			val secSinceLast_opt : Option[Float] = lastUpdateTime_opt.map(srcStampMsec - _)
			val minUpd : Float = 0.6f * currIntrstMsg.getUpdatePeriodSec
			if (secSinceLast_opt.isEmpty || (secSinceLast_opt.get > minUpd )) {
				val notice : ItemFieldDataMsg = buildCrudeNoticeFromData(currIntrstMsg, relevantData)
				val tlr = currIntrstMsg.getDownstreamTeller
				tlr.tellStrongCPMsg(notice)
				myLastUpdateSent_opt = Some(notice)
				true
			} else false
		} else false

	}

	def buildCrudeNoticeFromData(intReg : FieldInterestRegMsg,
								 updatedFieldsBreadthFirst : List[ItemFieldData]) : ItemFieldDataMsg = {

		null
	}
}

//
trait InterestRegistryLogic extends FieldDataFilterFuncs {
	val myInterestChansByID = new MutableHashMap[Ident, InterestChan]()
	val myInterestIdentsByFieldSpec = new MutableHashMap[ItemFieldSpecDirectImpl, Set[Ident]]()
	def addOrUpdateInterestReg(iirg : FieldInterestRegMsg) : Unit = {
		// TODO 1:  If there was an old version of this interest, and it has different fieldSpecs,
		// then we need to remove them from the fieldSpec keymap.
		// TODO 2:  We want to send a first update based on current state of the data.
		val regID = iirg.getInterestRegID
		val interestChan = new InterestChan(iirg)
		myInterestChansByID.put(regID, interestChan)
		val fieldSpecs : Traversable[ItemFieldSpec] = iirg.getInterestingFieldSpecs
		fieldSpecs.map(recordFSMapping(_, regID))
	}
	private def recordFSMapping(ifs : ItemFieldSpec, regID : Ident): Unit = {

	}

	def findMatchingIntrstChans(fieldSpec : ItemFieldSpec) :  Traversable[InterestChan] = {
		Nil
	}
	def publishNoticesForUpdatedData(sdmi : SourceDataMsg) : Int = {
		val fdbf : List[ItemFieldData] = sdmi.getFieldDataBreadthFirst
		val srcStampMsec : Long = sdmi.getSourceTStampMsec
		val topSpecs = fdbf.map(_.getFieldAddress)
		// Todo:  Look for deeper patterns of possibly matching specs in nested data, etc.
		// val bags = justFieldDataBags(fdbf)
		val matchingInterestChans = topSpecs.flatMap(findMatchingIntrstChans(_)).toList
		val sentFlags : List[Boolean] = matchingInterestChans.map(_.maybePublish(srcStampMsec, fdbf))
		val sentCount : Int = sentFlags.count(flg => flg)
		sentCount
	}
	// What does "changed" mean?  (On upstream side)
	// How do we ensure we can send initial state when downstream clients register interest?
	// How does downstream NavPageActor efficiently suspend/resume *all* its registrations?
}
class FieldDataOriginActor(logic : InterestRegistryLogic) extends FrienduActor {
	getLogger.info("In FieldDataDistributor actor constructor, logic={}", logic)
	def receive = {
		case regint : FieldInterestRegMsg => { // Usually connects us to some DistributorActor
			logic.addOrUpdateInterestReg(regint)
		}
		case sdmi : SourceDataMsg => { // Source data for us to route to appropriate distributors
			logic.publishNoticesForUpdatedData(sdmi)
		}

		case msg: AnyRef => {
			val msgDump = msg.toString()
			getLogger.info("TATDA received msg of clazz={} and dump-len={}", msg.getClass, msgDump.length)
			getLogger.debug("Received message dump:\n{}", msgDump)
		}
	}
}

trait ReportFilteringPolicy {
	// Empty => Reporter's discretion
	def itemsIncluded : Traversable[Ident] = Nil
	def itemsExcluded : Traversable[Ident] = Nil

	def fieldsIncluded : Traversable[Ident] = Nil
	def fieldsExcluded : Traversable[Ident] = Nil

	// Combos are higher priority, overriding any item/field settings
	def combosIncluded : Traversable[ItemFieldSpec] = Nil
	def combosExcluded : Traversable[ItemFieldSpec] = Nil
}
trait ReportingPolicy {
	// How is detail specified?   What combination of changed + unchanged field data?
	// Usually we want to send an initial exhaustive state report, followed by
	// smaller reports on changes.
	// How often?  What kind of throttling to use?

	def initialFilter_opt : Option[ReportFilteringPolicy] = Some(new ReportFilteringPolicy {})
	def updateFilter_opt : Option[ReportFilteringPolicy] = Some(new ReportFilteringPolicy {})
}
trait ReportOncePolicy extends ReportingPolicy {
	override def updateFilter_opt = None
}


class ScalaItemState
class RdfItemState