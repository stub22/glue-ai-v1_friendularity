package org.friendularity.vw.cli.goshcl

import org.appdapter.core.name.Ident
import org.cogchar.api.thing.ThingActionSpec
import org.cogchar.name.goody.GoodyNames
import org.friendularity.vw.msg.cor.VWContentRq

/**
  * Created by Owner on 1/22/2017.
  */
trait GoodyRqPartialXlator {
	def makeCreateRqs(verbID : Ident, tgtTypeID : Ident, tgtID : Ident) : List[VWContentRq] = Nil
	def makeDeleteRqs(verbID : Ident, tgtTypeID : Ident, tgtID : Ident) : List[VWContentRq] = Nil
	def makeMoveRqs(verbID : Ident, tgtTypeID : Ident, tgtID : Ident) : List[VWContentRq] = Nil
	def makeSetRqs(verbID : Ident, tgtTypeID : Ident, tgtID : Ident) : List[VWContentRq] = Nil
}
trait GoodyRqToShaperRqTranslator {

	lazy val myBitGoodyXlator = new BitGoodyShapeXlator {}
	lazy val myTicTacGoodyXlator = new TicTacShapeXlator {}
	lazy val myFlatGoodyXlator = new FlatGoodyShapeXlator {}
	lazy val myOtherGoodyXlator = new OtherGoodyShapeXlator {}

	def makeCreationRqsFromTA(actSpec : ThingActionSpec) : List[VWContentRq] = {
		val (verbID, tgtTypeID, tgtID) = (actSpec.getVerbID, actSpec.getTargetThingTypeID, actSpec.getTargetThingID)
		val msgList : List[VWContentRq] = tgtTypeID match {
			case GoodyNames.TYPE_BIT_BOX => {
				Nil
			}
		}
		msgList
	}
	def makeDelRqsFromTA	(actSpec : ThingActionSpec) : List[VWContentRq] = {
		val (verbID, tgtTypeID, tgtID) = (actSpec.getVerbID, actSpec.getTargetThingTypeID, actSpec.getTargetThingID)
		Nil
	}

	def makeMoveRqsFromTA(actSpec : ThingActionSpec) : List[VWContentRq] = {
		val (verbID, tgtTypeID, tgtID) = (actSpec.getVerbID, actSpec.getTargetThingTypeID, actSpec.getTargetThingID)
		// Thinking:  One level of discrim we may want here is between flat and deep
		Nil
	}
	def makeSetRqsFromTA(actSpec : ThingActionSpec) : List[VWContentRq] = {
		val (verbID, tgtTypeID, tgtID) = (actSpec.getVerbID, actSpec.getTargetThingTypeID, actSpec.getTargetThingID)
		Nil
	}

	protected def chooseXlatorByThingType(thingTypeID : Ident) : GoodyRqPartialXlator = {
		thingTypeID match {
			case GoodyNames.TYPE_BIT_BOX | GoodyNames.TYPE_BIT_CUBE => myBitGoodyXlator
			case GoodyNames.TYPE_TICTAC_GRID | GoodyNames.TYPE_TICTAC_MARK => myTicTacGoodyXlator
			case GoodyNames.TYPE_CROSSHAIR | GoodyNames.TYPE_SCOREBOARD | GoodyNames.TYPE_TEXT => myFlatGoodyXlator
			case _ => myOtherGoodyXlator
		}
	}
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