package org.friendularity.vw.cli.goshcl

import com.jme3.math.{ColorRGBA, Quaternion, Vector3f}
import org.appdapter.core.name.Ident
import org.appdapter.fancy.log.VarargsLogging
import org.cogchar.api.thing.{TypedValueMap, ThingActionSpec}
import org.cogchar.name.goody.GoodyNames
import org.friendularity.vw.msg.cor.VWContentRq
import org.friendularity.vwmsg.{OrdinaryParams3D, CoreParams3D}

/**
  * Created by Stub22 on 1/22/2017.
  */
trait GeneralXlatorSupport {
	def makeShapeXform : Unit = ???
	/*
	def makeCoreParams3D(paramTVM : TypedValueMap) : CoreParams3D = {
		val pos3f : Vector3f =
		val rotQuat : Quaternion
		val scale3f : Vector3f
		val myColor : ColorRGBA
		new OrdinaryParams3D(pos3f , rotQuat, scale3f, myColor)
	}
	*/

}
trait GoodyRqPartialXlator extends GeneralXlatorSupport {
	// These methods are responsible for setting of all params in the output requests.
	def makeCreateRqs(verbID : Ident, tgtTypeID : Ident, tgtID : Ident, paramTVM : TypedValueMap) : List[VWContentRq] = Nil
	def makeDeleteRqs(verbID : Ident, tgtID : Ident) : List[VWContentRq] = Nil
	def makeMoveRqs(verbID : Ident, tgtID : Ident, paramTVM : TypedValueMap) : List[VWContentRq] = Nil
	def makeSetRqs(verbID : Ident, tgtID : Ident, paramTVM : TypedValueMap) : List[VWContentRq] = Nil
}
import scala.collection.mutable.{Map => MutaMap, HashMap}

case class RecordedXlation(taRcvd: ThingActionSpec, shapeRqsSent : List[VWContentRq], tstmpMsec : Long)

case class MadeGoodyRec(goodyID : Ident, xlator : GoodyRqPartialXlator) {
	private var myRecordedXlations : List[RecordedXlation] = Nil

	def recordXlation(taRcvd : ThingActionSpec, shapeRqsSent : List[VWContentRq]) : Unit = {
		val tstmpMsec = System.currentTimeMillis()
		val xlt = new RecordedXlation(taRcvd, shapeRqsSent, tstmpMsec)
		myRecordedXlations = myRecordedXlations ::: List(xlt)
	}

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
}

trait SubTransChooser {
	lazy val myBitGoodyXlator = new BitGoodyShapeXlator {}
	lazy val myTicTacGoodyXlator = new TicTacShapeXlator {}
	lazy val myFlatGoodyXlator = new FlatGoodyShapeXlator {}
	lazy val myOtherGoodyXlator = new OtherGoodyShapeXlator {}

	protected def chooseXlatorByThingType(thingTypeID : Ident) : GoodyRqPartialXlator = {
		thingTypeID match {
			case GoodyNames.TYPE_BIT_BOX | GoodyNames.TYPE_BIT_CUBE => myBitGoodyXlator
			case GoodyNames.TYPE_TICTAC_GRID | GoodyNames.TYPE_TICTAC_MARK => myTicTacGoodyXlator
			case GoodyNames.TYPE_CROSSHAIR | GoodyNames.TYPE_SCOREBOARD | GoodyNames.TYPE_TEXT => myFlatGoodyXlator
			case _ => myOtherGoodyXlator
		}
	}

}
trait KnowsGoodyCache {
	protected def getMGCache : MadeGoodyCache
}
trait GoodyCreateRequestXlator extends SubTransChooser with KnowsGoodyCache with VarargsLogging {

	def makeCreationRqsFromTA(actSpec : ThingActionSpec) : List[VWContentRq] = {
		val (verbID, tgtTypeID, tgtID) = (actSpec.getVerbID, actSpec.getTargetThingTypeID, actSpec.getTargetThingID)
		debug3("Processing create for tgtID={}, tgtTypeID={}.  No fancy switching for verbID={}",  tgtTypeID, tgtID, verbID)
		val ourCache : MadeGoodyCache = getMGCache
		if (ourCache.hasMGRecAtID(tgtID)) {
			throw new RuntimeException("Attempted re-creation of already-made goody at ID=" + tgtID)
		}
		val xlator = chooseXlatorByThingType(tgtTypeID)
		val paramTVM = actSpec.getParamTVM
		val shapeRqList : List[VWContentRq] = xlator.makeCreateRqs(verbID, tgtTypeID, tgtID, paramTVM)
		val madeRec = new MadeGoodyRec(tgtID, xlator)
		ourCache.storeMGRecAtID(madeRec)
		madeRec.recordXlation(actSpec, shapeRqList)
		shapeRqList
	}
}
trait ExistingGoodyRqTranslator extends KnowsGoodyCache with VarargsLogging {

	protected def operateOnExistingGoody (actSpec : ThingActionSpec,
										  oper : Function2[MadeGoodyRec, ThingActionSpec, List[VWContentRq]]) : List[VWContentRq] = {

		val (verbID, tgtTypeID, tgtID) = (actSpec.getVerbID, actSpec.getTargetThingTypeID, actSpec.getTargetThingID)
		val ourCache : MadeGoodyCache = getMGCache
		if (ourCache.hasMGRecAtID(tgtID)) {
			val mgrec : MadeGoodyRec = ourCache.getMGRecAtID_opt(tgtID).get
			val shapeRqList = oper(mgrec, actSpec)
			mgrec.recordXlation(actSpec, shapeRqList)
			shapeRqList
		} else {
			error3("Cannot process TA-request with verb={}, goody not found at ID={}, TA={}", verbID, tgtID, actSpec)
			Nil
		}
	}
	// val (verbID, tgtTypeID, tgtID) = (actSpec.getVerbID, actSpec.getTargetThingTypeID, actSpec.getTargetThingID)
	// Thinking:  One level of discrim we may want here is between flat and deep

	protected def opDel(mgrec : MadeGoodyRec, taSpec : ThingActionSpec) : List[VWContentRq] = {
		mgrec.xlator.makeDeleteRqs(taSpec.getVerbID, taSpec.getTargetThingID)
	}
	protected def opMove(mgrec : MadeGoodyRec, taSpec : ThingActionSpec) : List[VWContentRq] = {
		mgrec.xlator.makeMoveRqs(taSpec.getVerbID, taSpec.getTargetThingID, taSpec.getParamTVM)
	}
	protected def opSet(mgrec : MadeGoodyRec, taSpec : ThingActionSpec) : List[VWContentRq] = {
		mgrec.xlator.makeSetRqs(taSpec.getVerbID, taSpec.getTargetThingID, taSpec.getParamTVM)
	}

	def makeDelRqsFromTA	(actSpec : ThingActionSpec) : List[VWContentRq] = {
		operateOnExistingGoody(actSpec, opDel)
	}
	def makeMoveRqsFromTA(actSpec : ThingActionSpec) : List[VWContentRq] = {
		operateOnExistingGoody(actSpec, opMove)
	}
	def makeSetRqsFromTA(actSpec : ThingActionSpec) : List[VWContentRq] = {
		operateOnExistingGoody(actSpec, opSet)
	}

}
trait GoodyRqToShaperRqTranslator extends KnowsGoodyCache with
			GoodyCreateRequestXlator with ExistingGoodyRqTranslator {

	val ourCacheIsStateYo = new MadeGoodyCache {}
	override protected def getMGCache : MadeGoodyCache = ourCacheIsStateYo

	def makeContentRqsFromTA(actSpec : ThingActionSpec, sttIsNeeded : Int) : List[VWContentRq] = {
		val (verbID, tgtTypeID, tgtID) = (actSpec.getVerbID, actSpec.getTargetThingTypeID, actSpec.getTargetThingID)
		verbID match {
			case GoodyNames.ACTION_CREATE => {
				makeCreationRqsFromTA(actSpec)
			}
			case GoodyNames.ACTION_DELETE => {
				makeDelRqsFromTA(actSpec)
			}
			case GoodyNames.ACTION_MOVE => {
				makeMoveRqsFromTA(actSpec)
			}
			case GoodyNames.ACTION_SET => {
				makeSetRqsFromTA(actSpec)
			}
		}
	}

}