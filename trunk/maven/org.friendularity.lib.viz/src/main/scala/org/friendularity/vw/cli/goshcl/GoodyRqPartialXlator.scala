package org.friendularity.vw.cli.goshcl

import com.jme3.math.ColorRGBA
import org.appdapter.core.name.Ident
import org.cogchar.api.thing.{ThingActionSpec, TypedValueMap}
import org.cogchar.name.goody.GoodyNames
import org.cogchar.render.app.entity.GoodyActionExtractor
import org.friendularity.vw.mprt.manip.{AbruptManipAbsPartialImpl, ManipDesc, MaybeTransform3D, MakesManipDesc}
import org.friendularity.vw.msg.cor.VWContentRq
import org.friendularity.vw.msg.shp.deep.{SimpleMatDesc, VWMatDesc, VWSCR_Node, ShapeManipRqImpl, VWShapeDeleteRq}

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
	def makeSetRqs(mgrec : MadeGoodyRec, taSpec : ThingActionSpec) // verbID : Ident, tgtID : Ident, paramTVM : TypedValueMap)
					 : List[VWContentRq] = {
		Nil
	}
	def makeParentCreateRqs_withXform(parentNodeShapeID : Ident, gparent_opt : Option[Ident], initXform_part : MaybeTransform3D) : List[VWContentRq] = {
		val initManipDesc : ManipDesc = new AbruptManipAbsPartialImpl(initXform_part)
		val parentCreateRq = new VWSCR_Node(parentNodeShapeID, gparent_opt)
		val statusTlr_opt = None
		val initManipRq = new ShapeManipRqImpl(parentNodeShapeID, initManipDesc, statusTlr_opt)
		val parentRqs = List[VWContentRq](parentCreateRq, initManipRq)
		parentRqs
	}

	def makeParentCreateRqs_withXform(parentNodeShapeID : Ident, gparent_opt : Option[Ident], taSpec : ThingActionSpec) : List[VWContentRq] = {
		val initXform_part = extractXform_part(taSpec)
		info1("Extracted initXform_part={}", initXform_part)
		makeParentCreateRqs_withXform(parentNodeShapeID, gparent_opt, initXform_part)
	}

	val DFLT_CLR = ColorRGBA.Gray
	def translateSimpleMatDesc(taSpec : ThingActionSpec) : VWMatDesc = {
		// possibleColorSrc
		val gax = new GoodyActionExtractor(taSpec)
		val colorSupplied_opt : Option[ColorRGBA] = extractColor(gax)
		val colorUsed = colorSupplied_opt.getOrElse(DFLT_CLR)
		new SimpleMatDesc(Some(colorUsed))

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
