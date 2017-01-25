package org.friendularity.vw.cli.goshcl

import org.appdapter.core.name.Ident
import org.cogchar.api.thing.{ThingActionSpec, TypedValueMap}
import org.cogchar.name.goody.GoodyNames
import org.cogchar.render.app.entity.GoodyActionExtractor
import org.friendularity.vw.mprt.manip.MakesManipDesc
import org.friendularity.vw.msg.cor.VWContentRq

/**
  * Created by Owner on 1/23/2017.
  */
trait GoodyRqPartialXlator extends GeneralXlatorSupport with MakesManipDesc {
	// These methods are responsible for setting of all params in the output requests.
	// def makeCreateRqs(verbID : Ident, tgtTypeID : Ident, tgtID : Ident, gax : GoodyActionExtractor) : List[VWContentRq] = Nil
	//
	def makeCreateRqs(taSpec : ThingActionSpec) : List[VWContentRq] = Nil
	def makeDeleteRqs(verbID : Ident, tgtID : Ident) : List[VWContentRq] = Nil
	def makeMoveRqs(verbID : Ident, tgtID : Ident, paramTVM : TypedValueMap) : List[VWContentRq] = Nil
	def makeSetRqs(verbID : Ident, tgtID : Ident, paramTVM : TypedValueMap) : List[VWContentRq] = Nil
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
