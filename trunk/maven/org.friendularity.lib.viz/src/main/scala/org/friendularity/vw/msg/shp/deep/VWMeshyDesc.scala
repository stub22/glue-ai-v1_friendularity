package org.friendularity.vw.msg.shp.deep

import com.jme3.math.{ColorRGBA, Quaternion, Vector3f}
import org.friendularity.vw.mprt.manip.MaybeTransform3D
import org.friendularity.vwmsg.CoreParams3D

/**
  * File created with code moved here from VWShapeMsg.scala by Stub22 on 1/17/2017.
  */

trait VWMatDesc { // } extends VWShapeCreateRq {

//	def getCoreParams3D_opt : Option[CoreParams3D] = None
	def getColorParam_opt : Option[ColorRGBA] = None // getCoreParams3D_opt.map(_.getColor)
}
trait VWMeshDesc {

}
case class SimpleMatDesc(myColor_opt : Option[ColorRGBA]) extends VWMatDesc {
	override def getColorParam_opt : Option[ColorRGBA] = myColor_opt
}
abstract class BaseMeshDesc() extends VWMeshDesc {
}
trait CompoundMeshyShapeRq extends VWShapeCreateRq {
	def getKnownIdentsPart : KnowsShapeIDsPart
	def getMeshDescPart : VWMeshDesc
	def getMatDescPart : VWMatDesc

	override def getKnownID_opt  = getKnownIdentsPart.getKnownID_opt
	override def getKnownParentID_opt  = getKnownIdentsPart.getKnownParentID_opt

	override def getColorParam_opt = getMatDescPart.getColorParam_opt

	//	override def getCoreParams3D_opt = getMeshyDescPart.getCoreParams3D_opt
	// override def getPosParam_opt  = getMeshyDescPart.getPosParam_opt
	// override def getRotParam_opt  = getMeshyDescPart.getRotParam_opt

}


// Arguable whether this compound msg should be a case class, with a VWSCR_ name.  Hmmm.
case class VWSCR_MeshyCmpnd(idsPart : KnowsShapeIDsPart, initXform3D : MaybeTransform3D,
							meshyPart : VWMeshDesc, matPart : VWMatDesc)
			extends  CompoundMeshyShapeRq {

	override def getKnownIdentsPart: KnowsShapeIDsPart = idsPart

	override def getInitXform3D_partial : MaybeTransform3D = initXform3D

	override def getMeshDescPart: VWMeshDesc = meshyPart

	override def getMatDescPart : VWMatDesc = matPart
	// override def getInitXform3D_partial : MaybeTransform3D = initXform3D
}

// Regular shape kinds:  Each of these is the inner-meshy part of a message to create one of the JME3 primitive
// shape kinds.

// class MeshyPrimMsgBase(partXForm : def getPaMaybeTransform3D)
case class VWSCR_Sphere(myRadius : Float) extends BaseMeshDesc() {

	// override def getCoreParams3D_opt : Option[CoreParams3D] = Option(myCoreParams)

}


case class VWSCR_Box() extends VWMeshDesc

// 2017-01-22 maps to 5-arg version of Cylinder constructor.
// Other 2 poss. args not included, yet, are:  float radius2, boolean inverted
case class VWSCR_Cylinder(axisSamples: Int, radialSamples: Int, radius: Float, height: Float, closed: Boolean) extends VWMeshDesc

case class VWSCR_Torus() extends VWMeshDesc

case class VWSCR_PQTorus() extends VWMeshDesc

case class VWSCR_Quad() extends VWMeshDesc
