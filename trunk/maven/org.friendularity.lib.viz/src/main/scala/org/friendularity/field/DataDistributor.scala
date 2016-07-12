package org.friendularity.field

import org.appdapter.core.name.Ident
import org.appdapter.fancy.log.VarargsLogging
import org.friendularity.cpump.CPMsgTeller
import org.friendularity.dull.FrienduActor

import scala.collection.mutable.{HashMap => MutableHashMap}

/**
  * Created by Stub22 on 7/11/2016.
  */

trait FieldDataDistributorLogic extends VarargsLogging {
	def findListenersForFieldDataLeaf(ifs: ItemFieldSpec) : Traversable[Function1[FieldDataLeaf, Unit]]

	def distributeUpdateLeaf(fdl : FieldDataLeaf) : Unit = {
		val listeners = findListenersForFieldDataLeaf(fdl.getFieldAddress)
		listeners.map(_.apply(fdl))
	}

	def distributeUpdateBag(bdl: FieldDataBag) : Unit = {
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

class MutableFieldDataDistrib() extends FieldDataDistributorLogic {

	val myLeafCallbacksByFieldSpec = new MutableHashMap[ItemFieldSpecDirectImpl,
				List[Function1[FieldDataLeaf, Unit]]]

	override def findListenersForFieldDataLeaf(ifs: ItemFieldSpec): Traversable[(FieldDataLeaf) => Unit] = ???

}
trait FlowControlState {
	private var myState : Boolean = false
	def processFlowCtrlMsg(dfcm : DistribFlowControlMsg) : Unit = {
		myState = dfcm.active
	}
}

trait UpstreamOrganizer {
	val myUpstreamTellersByItemID = new MutableHashMap[Ident, CPMsgTeller]
	def getUniqueUpstreamTellers : Set[CPMsgTeller] = myUpstreamTellersByItemID.values.toSet

	def getDownstreamTeller : CPMsgTeller

	def registerInterest(): Unit = {
		def downstream = getDownstreamTeller

	}
}

class FieldDataDistributorActor(logic : FieldDataDistributorLogic) extends FrienduActor {
	getLogger.info("In FieldDataDistributor actor constructor, logic={}", logic)
	def receive = {
		case regint : FieldInterestRegMsg => {

		}
		case ifdm : ItemFieldDataMsg => {
		}

		case msg: AnyRef => {
			val msgDump = msg.toString()
			getLogger.info("TATDA received msg of clazz={} and dump-len={}", msg.getClass, msgDump.length)
			getLogger.debug("Received message dump:\n{}", msgDump)
		}
	}
}


