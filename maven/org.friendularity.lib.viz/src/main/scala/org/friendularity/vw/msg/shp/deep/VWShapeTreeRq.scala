package org.friendularity.vw.msg.shp.deep

import org.appdapter.core.name.Ident
import org.friendularity.vw.msg.cor.VWContentRq

/**
  * Created by Owner on 1/31/2017.
  */
trait VWShapeTreeRq extends VWContentRq {

}

trait VWApplyMatToAll extends VWShapeTreeRq {
	def getTopShapeID : Ident
	def getMatDesc : VWMatDesc
}
case class VWApplyMatToShapeTree(topShapeID : Ident, matDesc : VWMatDesc) extends VWApplyMatToAll {
	override def getTopShapeID : Ident = topShapeID
	override def getMatDesc : VWMatDesc = matDesc

}