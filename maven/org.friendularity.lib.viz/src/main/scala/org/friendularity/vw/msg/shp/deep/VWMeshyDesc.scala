package org.friendularity.vw.msg.shp.deep

import com.jme3.material.Material
import com.jme3.math.{ColorRGBA, Quaternion, Vector3f}
import org.cogchar.render.sys.registry.RenderRegistryClient
import org.friendularity.vw.mprt.manip.MaybeTransform3D
import org.friendularity.vwmsg.CoreParams3D

/**
  * File created with code moved here from VWShapeMsg.scala by Stub22 on 1/17/2017.
  *
  * Copying header comment from there from June '16
  *
  * See both the "shape" and "debug" packages:
  * https://github.com/jMonkeyEngine/jmonkeyengine/tree/master/jme3-core/src/main/java/com/jme3/scene/shape
  * https://github.com/jMonkeyEngine/jmonkeyengine/tree/master/jme3-core/src/main/java/com/jme3/scene/debug
  *
  *...and this Cogchar comment:
  * Mesh has these direct subclasses for simpler shapes:
  * AbstractBox, Arrow, Curve, Cylinder, Dome, Grid, Line, ParticleMesh, PQTorus, Quad, SkeletonPoints,
  * SkeletonWire, Sphere, Surface, Torus, WireBox, WireFrustum, WireSphere
  */

trait VWMatDesc { // } extends VWShapeCreateRq {

//	def getCoreParams3D_opt : Option[CoreParams3D] = None
	def getColorParam_opt : Option[ColorRGBA] = None // getCoreParams3D_opt.map(_.getColor)

	def makeSpecialMaterial_opt(rrc : RenderRegistryClient) : Option[Material] = None
}
trait VWMeshDesc {

}
case class SimpleMatDesc(myColor_opt : Option[ColorRGBA]) extends VWMatDesc {
	override def getColorParam_opt : Option[ColorRGBA] = myColor_opt
}
abstract class BaseMeshDesc() extends VWMeshDesc {
}
// Regular shape kinds:  Each of these is the inner-meshy part of a message to create one of the JME3 primitive
// shape kinds.

// class MeshyPrimMsgBase(partXForm : def getPaMaybeTransform3D)
case class VWMD_Sphere(zSamples : Int, radialSamples : Int,  radiusF : Float) extends BaseMeshDesc()

case class VWMD_Box(xSize : Float, ySize : Float, zSize : Float) extends VWMeshDesc

case class VWMD_TexturedBox(xSize : Float, ySize : Float, zSize : Float) extends VWMeshDesc

// 2017-01-22 maps to 5-arg version of Cylinder constructor.
// Other 2 poss. args not included, yet, are:  float radius2, boolean inverted
case class VWMD_Cylinder(axisSamples: Int, radialSamples: Int, radius: Float, height: Float, closed: Boolean) extends VWMeshDesc

case class VWMD_Torus(circleSamples : Int, radialSamples : Int, innerRadius : Float, outerRadius : Float) extends VWMeshDesc

case class VWMD_PQTorus(p : Float, q : Float, radius : Float, width : Float, steps : Int, radialSamples : Int) extends VWMeshDesc

case class VWMD_Quad() extends VWMeshDesc
