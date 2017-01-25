package org.friendularity.vw.cli.goshcl

import com.jme3.math.ColorRGBA
import org.appdapter.core.name.Ident
import org.cogchar.api.thing.{ThingActionSpec, TypedValueMap}
import org.cogchar.name.goody.GoodyNames
import org.cogchar.render.app.entity.GoodyActionExtractor
import org.friendularity.vw.msg.cor.VWContentRq

/**
  * Created by Owner on 1/22/2017.
  */
trait OtherGoodyShapeXlator extends GoodyRqPartialXlator {
	private val DEFAULT_FLOOR_COLOR: ColorRGBA = ColorRGBA.LightGray

	override def makeCreateRqs(taSpec : ThingActionSpec) : List[VWContentRq]  = {
//	override def makeCreateRqs(verbID : Ident, tgtTypeID : Ident, tgtID : Ident,  gax : GoodyActionExtractor) // paramTVM : TypedValueMap)
//				: List[VWContentRq] = {
		val tgtTypeID : Ident = taSpec.getTargetThingTypeID
		val msgList : List[VWContentRq] = tgtTypeID match {
			case GoodyNames.TYPE_BOX => {
				Nil
			}
			case GoodyNames.TYPE_FLOOR => {
				makeRqs_floor
			}
		}
		msgList
	}

	def makeRqs_floor : List[VWContentRq] = {
		Nil
	}

}
