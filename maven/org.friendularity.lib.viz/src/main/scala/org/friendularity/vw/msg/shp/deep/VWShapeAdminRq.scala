package org.friendularity.vw.msg.shp.deep

import org.appdapter.core.name.Ident
import org.friendularity.vw.msg.cor.VWContentRq

/**
  * Created by Owner on 1/23/2017.
  */
case class VWShapeAttachRq(knownID : Ident, knownParentID_opt : Option[Ident]) extends VWContentRq

trait VWShapeClearRq extends VWContentRq

case class VWClearAllShapes() extends VWShapeClearRq

case class VWShapeDetachRq(shapeID : Ident) extends VWContentRq

