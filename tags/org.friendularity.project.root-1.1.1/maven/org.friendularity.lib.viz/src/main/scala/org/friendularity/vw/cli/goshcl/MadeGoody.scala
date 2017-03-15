package org.friendularity.vw.cli.goshcl

import org.appdapter.core.name.Ident
import org.cogchar.api.thing.ThingActionSpec
import org.friendularity.vw.msg.cor.VWContentRq
import org.friendularity.vw.msg.shp.deep.{VWShapeCreateRq, VWSCR_Node}

import scala.collection.mutable.{Map => MutaMap, HashMap}
/**
  * Created by Owner on 1/23/2017.8
  */
case class RecordedXlation(taRcvd: ThingActionSpec, shapeRqsSent : List[VWContentRq], tstmpMsec : Long)

case class MadeGoodyRec(goodyID : Ident, xlator : GoodyRqPartialXlator) {
	private var myDeletedFlag : Boolean = false

	private var myRecordedXlations : List[RecordedXlation] = Nil

	def recordXlation(taRcvd : ThingActionSpec, shapeRqsSent : List[VWContentRq]) : Unit = {
		val tstmpMsec = System.currentTimeMillis()
		val xlt = new RecordedXlation(taRcvd, shapeRqsSent, tstmpMsec)
		myRecordedXlations = myRecordedXlations ::: List(xlt)
	}
	def getFirstTA : Option[ThingActionSpec] = {
		myRecordedXlations.headOption.map(_.taRcvd)
	}

	def getFirstTgtTypeID : Option[Ident] = {
		getFirstTA.map(_.getTargetThingTypeID)
	}

	def getTopShapeID : Ident = {
		val firstXlation : RecordedXlation = myRecordedXlations.head
		val firstRqSent : VWContentRq  = firstXlation.shapeRqsSent.head
		firstRqSent.asInstanceOf[VWShapeCreateRq].getKnownID_opt.get
	}

	def getAllShapeIDs : List[Ident] = Nil

	def isMarkedDeleted : Boolean = myDeletedFlag

	def markDeleted : Unit = {
		myDeletedFlag = true
	}

	// TODO:  Need something like this to find old tic-tac mark to be replaced.
	def getAllSetXlationsMatchingParams : Unit = {}

}
trait MadeGoodyCache {
	lazy val myMGRecsByID : MutaMap[Ident, MadeGoodyRec] = new HashMap[Ident, MadeGoodyRec]
	def hasMGRecAtID(goodyID : Ident) : Boolean = myMGRecsByID.contains(goodyID)

	def getMGRecAtID_opt(goodyID : Ident) : Option[MadeGoodyRec] = myMGRecsByID.get(goodyID)

	def storeMGRecAtID(mgrec : MadeGoodyRec) : Unit = storeMGRecAtID(mgrec.goodyID, mgrec)

	private def storeMGRecAtID(goodyID : Ident, mgrec : MadeGoodyRec) : Unit = {
		if (hasMGRecAtID(goodyID)) {
			throw new RuntimeException("Egads - cannot store a goody where one exists at ID=" + goodyID)
		}
		myMGRecsByID.put(goodyID, mgrec)
	}

	def logicallyDeleteMGRec(mgrec : MadeGoodyRec) : Unit = {
		mgrec.markDeleted
		// DECIDE:  Do we want goodies to go away
	}
}
trait KnowsGoodyCache {
	protected def getMGCache : MadeGoodyCache
}
