package org.friendularity.vw.cli.goshcl

import org.appdapter.core.name.Ident
import org.cogchar.api.thing.{ThingActionSpec, TypedValueMap}
import org.cogchar.name.goody.GoodyNames
import org.cogchar.render.app.entity.GoodyActionExtractor
import org.friendularity.vw.mprt.manip.{MaybeTransform3D, MakesManipDesc}
import org.friendularity.vw.msg.cor.VWContentRq
import org.friendularity.vw.msg.shp.deep.{ShapeManipRqImpl, VWShapeDeleteRq}

import java.lang.{Float => JFloat, Integer => JInt, Long => JLong}


/**
  * Created by Owner on 1/23/2017.
  */
trait GoodyRqPartialXlator extends GeneralXlatorSupport with MakesManipDesc {
	// These methods are responsible for setting of all params in the output requests.
	// def makeCreateRqs(verbID : Ident, tgtTypeID : Ident, tgtID : Ident, gax : GoodyActionExtractor) : List[VWContentRq] = Nil
	//
	def makeCreateRqs(taSpec : ThingActionSpec) : List[VWContentRq] = Nil
	def makeDeleteRqs(mgrec : MadeGoodyRec, verbID : Ident, tgtID : Ident) : List[VWContentRq] = {
		val topShapeID = mgrec.getTopShapeID
		val delTS = new VWShapeDeleteRq(topShapeID)
		List(delTS)
	}
	def makeMoveRqs(mgrec : MadeGoodyRec, taSpec : ThingActionSpec) : List[VWContentRq] = {
		// verbID : Ident, tgtID : Ident, paramTVM : TypedValueMap
		val paramTVM : TypedValueMap = taSpec.getParamTVM
		val maybeXform : MaybeTransform3D = extractXform_part(taSpec)
		val durSec_opt : Option[JFloat] = extractDuration(paramTVM)
		val topShapeID = mgrec.getTopShapeID


		val forceToFullXform = false // "Partial" approach is preferred as of 2016-Nov, see RVWS-49 and RVWS-57.
		val manipGuts = makeManipGuts(maybeXform, durSec_opt, forceToFullXform)
		val statusTlr_opt = None
		val shapeManipRq = new ShapeManipRqImpl(topShapeID, manipGuts, statusTlr_opt)
		List(shapeManipRq)
	}
	def makeSetRqs(mgrec : MadeGoodyRec, verbID : Ident, tgtID : Ident, paramTVM : TypedValueMap) : List[VWContentRq] = Nil
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
