package org.friendularity.field

import org.appdapter.core.name.Ident
import org.friendularity.dull.FrienduActor

import scala.collection.mutable.{HashMap => MutableHashMap}

/**
  * Created by Stub22 on 7/11/2016.
  */
class InterestNotifyChan(initialInterestMsg : FieldInterestRegMsg) {
	private var myLastRegMsg : FieldInterestRegMsg = initialInterestMsg
	private var myLastUpdateSent_opt : Option[ItemFieldDataMsg] = None

	// Return value indicates whether we published, allowing caller to count messages sent.
	def maybePublish() : Boolean = {

		false
	}

}

//
trait InterestRegistryLogic extends FieldDataFilterFuncs {
	val myInterestsByID = new MutableHashMap[Ident, FieldInterestRegMsg]()
	val myInterestIdentsByFieldSpec = new MutableHashMap[ItemFieldSpecDirectImpl, Set[Ident]]()
	def addOrUpdateInterestReg(iirg : FieldInterestRegMsg) : Unit = {
		// TODO:  If there was an old version of this interest, and it has different fieldSpecs,
		// then we need to remove them from the fieldSpec keymap.
		val regID = iirg.getInterestRegID
		myInterestsByID.put(regID, iirg)
		val fieldSpecs : Traversable[ItemFieldSpec] = iirg.getInterestingFieldSpecs
		fieldSpecs.map(recordFSMapping(_, regID))
	}
	private def recordFSMapping(ifs : ItemFieldSpec, regID : Ident): Unit = {

	}

	def findMatchingSpecificInterests(fieldSpec : ItemFieldSpec) :  Traversable[FieldInterestRegMsg] = {
		Nil
	}
	def buildCrudeNoticeFromData(intReg : FieldInterestRegMsg,
								 updatedFieldsBreadthFirst : List[ItemFieldData]) : ItemFieldDataMsg = {

		null
	}
	def publishNoticesForUpdatedData(sdmi : SourceDataMsgImpl) : Unit = {
		val fdbf = sdmi.getFieldDataBreadthFirst
		val topSpecs = fdbf.map(_.getFieldAddress)
		val matchingInterests = topSpecs.flatMap(findMatchingSpecificInterests(_)).toSet
		for (mi <- matchingInterests) {

			val relevantData = fdbf.filter(mi.isInterestedIn(_))
			val notice = buildCrudeNoticeFromData(mi, relevantData)
			val tlr = mi.getInterestedTeller
		}
		val bags = justFieldDataBags(fdbf)

	}
	// What does "changed" mean?  (On upstream side)
	// How do we ensure we can send initial state when downstream clients register interest?
	// How does downstream NavPageActor efficiently suspend/resume *all* its registrations?
}
class FieldDataOriginActor(logic : InterestRegistryLogic) extends FrienduActor {
	getLogger.info("In FieldDataDistributor actor constructor, logic={}", logic)
	def receive = {
		case regint : FieldInterestRegMsg => {
			logic.addOrUpdateInterestReg(regint)
		}
		case sdmi : SourceDataMsgImpl => {
		}

		case msg: AnyRef => {
			val msgDump = msg.toString()
			getLogger.info("TATDA received msg of clazz={} and dump-len={}", msg.getClass, msgDump.length)
			getLogger.debug("Received message dump:\n{}", msgDump)
		}
	}
}
