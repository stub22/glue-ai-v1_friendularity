package org.friendularity.vw.msg.shp.deep

import com.jme3.math.{ColorRGBA, Quaternion, Vector3f}
import org.friendularity.vwmsg.CoreParams3D

/**
  * File created with code moved here from VWShapeMsg.scala by Stub22 on 1/17/2017.
  */

trait VWMeshyShapeRq extends VWShapeCreateRq {
	def getCoreParams3D : Option[CoreParams3D] = None
	def getColorParam : Option[ColorRGBA] = getCoreParams3D.map(_.getColor)
	def getPosParam : Option[Vector3f] = getCoreParams3D.map(_.getPos)
	def getRotParam : Option[Quaternion] = getCoreParams3D.map(_.getRotQuat)
}

trait TwoPartMeshyShapeRq extends VWShapeCreateRq with VWMeshyShapeRq  {
	def getKnownIdentsPart : VWShapeCreateRq
	def getMeshyDescPart : VWMeshyShapeRq

	override def getKnownID_opt  = getKnownIdentsPart.getKnownID_opt
	override def getKnownParentID_opt  = getKnownIdentsPart.getKnownParentID_opt

	override def getCoreParams3D = getMeshyDescPart.getCoreParams3D
	override def getColorParam = getMeshyDescPart.getColorParam
	override def getPosParam  = getMeshyDescPart.getPosParam
	override def getRotParam  = getMeshyDescPart.getRotParam

}
// Arguable whether this compound msg should be a case class, with a VWSCR_ name.  Hmmm.
case class VWSCR_MeshyCmpnd(idsPart : VWShapeCreateRq, meshyPart : VWMeshyShapeRq) extends TwoPartMeshyShapeRq {
	override def getKnownIdentsPart: VWShapeCreateRq = idsPart

	override def getMeshyDescPart: VWMeshyShapeRq = meshyPart
}

// Regular shape kinds:  Each of these is the inner-meshy part of a message to create one of the JME3 primitive
// shape kinds.
case class VWSCR_Sphere(myRadius : Float, myCoreParams : CoreParams3D) extends VWMeshyShapeRq {

	override def getCoreParams3D : Option[CoreParams3D] = Option(myCoreParams)

}


case class VWSCR_Box() extends VWMeshyShapeRq

// 2017-01-22 maps to 5-arg version of Cylinder constructor.
// Other 2 poss. args not included, yet, are:  float radius2, boolean inverted
case class VWSCR_Cylinder(axisSamples: Int, radialSamples: Int, radius: Float, height: Float, closed: Boolean) extends VWMeshyShapeRq

case class VWSCR_Torus() extends VWMeshyShapeRq

case class VWSCR_PQTorus() extends VWMeshyShapeRq

case class VWSCR_Quad() extends VWMeshyShapeRq
