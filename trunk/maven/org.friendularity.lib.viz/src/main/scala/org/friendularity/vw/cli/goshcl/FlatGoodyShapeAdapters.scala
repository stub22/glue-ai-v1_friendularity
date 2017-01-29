package org.friendularity.vw.cli.goshcl

import com.jme3.math.ColorRGBA
import org.cogchar.api.thing.ThingActionSpec
import org.cogchar.name.goody.GoodyNames
import org.cogchar.render.app.entity.GoodyActionExtractor
import org.friendularity.vw.msg.cor.VWContentRq
import org.friendularity.vw.msg.shp.deep.VWSCR_TextBox

/**
  * Created by Owner on 1/22/2017.
  */
trait FlatGoodyShapeXlator extends GoodyRqPartialXlator {

	override def makeCreateRqs(taSpec: ThingActionSpec): List[VWContentRq] = {


		val inFlatSpc : Boolean = true
		val gax = new GoodyActionExtractor(taSpec)
		val xform = extractXform(taSpec.getParamTVM, gax)
		val txtContent = gax.getSpecialString(GoodyNames.TEXT) //  "This is the text"
		info1("Extracted txtContent={}", txtContent)
		val color_opt : Option[ColorRGBA] = extractColor(gax)
		val txtBxRq = new VWSCR_TextBox(txtContent, inFlatSpc, xform, color_opt.getOrElse(ColorRGBA.Black))
		List(txtBxRq)
	}
}